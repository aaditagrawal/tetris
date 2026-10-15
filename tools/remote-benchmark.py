#!/usr/bin/env python3
"""Simulate limited terminal output bandwidth; measure input-to-visible-pause.

This is a local PTY simulation, not a measurement of an actual SSH connection.
No scores or settings in the user's session are changed. Linux/macOS, stdlib only.
"""

import argparse
import errno
import fcntl
import json
import os
import pty
import select
import signal
import struct
import tempfile
import termios
import time


def measure(binary, fps, output_rate, input_rate, duration):
    with tempfile.TemporaryDirectory(prefix="tetris-remote-benchmark-") as directory:
        pid, fd = pty.fork()
        if not pid:
            fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 80, 0, 0))
            env = dict(os.environ, TERM="xterm-256color", XDG_DATA_HOME=directory)
            env.pop("GHCRTS", None)
            os.execve(binary, [binary, "--level", "0", "--fps", str(fps)], env)

        output = bytearray()
        budget = 0
        last_read = time.monotonic()
        reaped = False

        def drain(throttle=True):
            nonlocal budget, last_read
            now = time.monotonic()
            budget = min(output_rate * 0.1, budget + (now - last_read) * output_rate)
            last_read = now
            if not select.select([fd], [], [], 0.001)[0]:
                return
            size = min(4096, int(budget)) if throttle else 65536
            if size < 1:
                time.sleep(0.0005)
                return
            try:
                data = os.read(fd, size)
            except OSError as exc:
                if exc.errno == errno.EIO:
                    return
                raise
            output.extend(data)
            if throttle:
                budget -= len(data)

        try:
            deadline = time.monotonic() + 5
            while b"Tetris" not in output:
                if time.monotonic() > deadline:
                    raise RuntimeError("Game did not draw within 5s")
                drain(False)
            os.write(fd, b"jjj")
            deadline = time.monotonic() + 0.05
            while time.monotonic() < deadline:
                drain(False)

            started = time.monotonic()
            last_read = started
            budget = 0
            sent = 0
            first = len(output)
            while time.monotonic() - started < duration:
                if time.monotonic() - started >= sent / input_rate:
                    os.write(fd, b"h" if sent // 7 % 2 == 0 else b"l")
                    sent += 1
                drain()

            paused_at = time.monotonic()
            marker = len(output)
            os.write(fd, b"p")
            while b"Paused" not in output[marker:]:
                if time.monotonic() - paused_at > 10:
                    raise RuntimeError("Visible pause took over 10s")
                drain()
            pause_ms = (time.monotonic() - paused_at) * 1000
            received = len(output) - first

            os.write(fd, b"q")
            deadline = time.monotonic() + 5
            while time.monotonic() < deadline:
                drain(False)
                child, status = os.waitpid(pid, os.WNOHANG)
                if child:
                    reaped = True
                    if os.waitstatus_to_exitcode(status) != 0:
                        raise RuntimeError("Game exited unsuccessfully")
                    break
            else:
                raise RuntimeError("Game did not quit within 5s")
            return {
                "fps_target": fps,
                "output_limit_bytes_sec": output_rate,
                "duration_s": duration,
                "input_rate_target": input_rate,
                "input_keys": sent,
                "pause_response_ms": round(pause_ms, 2),
                "bytes_received_through_pause": received,
            }
        finally:
            if not reaped:
                try:
                    os.kill(pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                os.waitpid(pid, 0)
            os.close(fd)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("binary", type=os.path.abspath)
    parser.add_argument("--fps", type=int, nargs="+", default=[120, 60])
    parser.add_argument("--output-rate", type=int, default=40000, help="bytes/sec")
    parser.add_argument("--input-rate", type=int, default=1000, help="keys/sec")
    parser.add_argument("--duration", type=float, default=8)
    parser.add_argument("--runs", type=int, default=2)
    args = parser.parse_args()
    if args.output_rate <= 0 or args.input_rate <= 0 or args.duration <= 0 or args.runs < 1:
        parser.error("Rates, duration, and runs must be positive")
    if not args.fps or any(fps < 0 or fps > 1000 for fps in args.fps):
        parser.error("FPS must be from 0 to 1000")
    for _ in range(args.runs):
        for fps in args.fps:
            print(json.dumps(measure(args.binary, fps, args.output_rate,
                                     args.input_rate, args.duration)), flush=True)


if __name__ == "__main__":
    main()
