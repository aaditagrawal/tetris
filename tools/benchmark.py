#!/usr/bin/env python3
"""Measure the real CLI in an isolated 80x30 PTY (Linux/macOS, stdlib only).

Usage: python3 tools/benchmark.py /path/to/tetris [--rts=-N1] [--runs 3]
No scores or terminal settings in the user's session are changed.
"""

import argparse
import errno
import fcntl
import json
import os
import pty
import re
import select
import signal
import statistics
import struct
import tempfile
import termios
import time


def measure(binary, scenario, rts, duration, level=0, fps=None, input_rate=50):
    with tempfile.TemporaryDirectory(prefix="tetris-benchmark-") as data_dir:
        started = time.monotonic()
        pid, fd = pty.fork()
        if pid == 0:
            fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 80, 0, 0))
            env = dict(os.environ, TERM="xterm-256color", XDG_DATA_HOME=data_dir)
            env.pop("GHCRTS", None)
            options = [] if fps is None else ["--fps", str(fps)]
            args = [binary, "--level", str(level), *options, "+RTS", "-s", *rts, "-RTS"]
            os.execve(binary, args, env)

        output = bytearray()
        reaped = False

        def drain(timeout):
            if not select.select([fd], [], [], max(0, timeout))[0]:
                return b""
            try:
                chunk = os.read(fd, 65536)
            except OSError as exc:
                if exc.errno == errno.EIO:
                    return b""
                raise
            output.extend(chunk)
            # Answer terminal queries, if the backend asks for capabilities.
            if b"\x1b[6n" in chunk:
                os.write(fd, b"\x1b[1;1R")
            return chunk

        try:
            while b"Tetris" not in output:
                drain(0.02)
                if time.monotonic() - started > 10:
                    raise RuntimeError("Game did not draw within 10s: " + repr(output[-500:]))
            startup_ms = (time.monotonic() - started) * 1000
            # Put the piece on screen and pause during setup. Active movement
            # alternates around the spawn column regardless of the random shape.
            os.write(fd, b"jjjp")
            settle = time.monotonic() + 0.15
            while time.monotonic() < settle:
                drain(0.01)
            if scenario in ("active", "gravity", "frames"):
                os.write(fd, b"p")
                settle = time.monotonic() + 0.05
                while time.monotonic() < settle:
                    drain(0.005)
            begin = time.monotonic()
            next_key = begin
            latencies = []
            pending = None
            sent = 0
            workload_start = len(output)
            while time.monotonic() - begin < duration:
                now = time.monotonic()
                if scenario in ("active", "ignored", "frames") and now >= next_key:
                    # Paused movements exercise no-op events; active movements
                    # alternate directions and always fit around spawn column.
                    direction = sent // 7 if scenario == "frames" else sent
                    os.write(fd, b"h" if direction % 2 == 0 else b"l")
                    # A coalesced frame represents many inputs; assigning its
                    # latency to the last key would hide the earlier keys' wait.
                    pending = None if scenario == "frames" else time.monotonic()
                    sent += 1
                    next_key += 1 / input_rate
                elif scenario == "gravity" and now >= next_key:
                    # Keep fast-level gravity running instead of benchmarking
                    # an already-ended game. Restart before the stack fills.
                    os.write(fd, b"r")
                    sent += 1
                    next_key += 0.2
                timeout = 0.002 if scenario == "paused" else min(0.002, max(0, next_key - time.monotonic()))
                chunk = drain(timeout)
                if chunk and pending is not None:
                    latencies.append((time.monotonic() - pending) * 1000)
                    pending = None
            elapsed = time.monotonic() - begin
            workload_output = output[workload_start:]
            # Vty emits one hide-cursor control sequence per terminal update.
            # Count redraw submissions, independently of the physical display.
            rendered_frames = workload_output.count(b"\x1b[?25l")
            os.write(fd, b"q")
            deadline = time.monotonic() + 5
            while True:
                drain(0.01)
                child, status, usage = os.wait4(pid, os.WNOHANG)
                if child:
                    reaped = True
                    while drain(0):
                        pass
                    break
                if time.monotonic() > deadline:
                    raise RuntimeError("Game did not quit within 5s")
            if os.waitstatus_to_exitcode(status) != 0:
                raise RuntimeError("Game failed: " + output.decode(errors="replace")[-2000:])
            stats = output.decode(errors="replace")
            allocated = re.search(r"([\d,]+) bytes allocated in the heap", stats)
            return {
                "startup_ms": round(startup_ms, 3),
                "cpu_ms": round((usage.ru_utime + usage.ru_stime) * 1000, 3),
                "max_rss_kib": usage.ru_maxrss / 1024 if os.uname().sysname == "Darwin" else usage.ru_maxrss,
                "allocated_bytes": int(allocated[1].replace(",", "")) if allocated else None,
                "response_p50_ms": round(statistics.median(latencies), 3) if latencies else None,
                "response_p95_ms": round(sorted(latencies)[int((len(latencies) - 1) * 0.95)], 3) if latencies else None,
                "responses": len(latencies),
                "keys_sent": sent,
                "output_bytes": len(output),
                "workload_output_bytes": len(workload_output),
                "rendered_frames": rendered_frames,
                "rendered_fps": round(rendered_frames / elapsed, 2),
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
    parser.add_argument("--rts", default="", help="e.g. --rts=-N1")
    parser.add_argument("--runs", type=int, default=3)
    parser.add_argument("--duration", type=float, default=3)
    parser.add_argument("--level", type=int, default=0)
    parser.add_argument("--fps", type=int, help="pass a redraw target to the game (0 is uncapped)")
    parser.add_argument("--input-rate", type=int, help="keys/sec; defaults to 1000 for frames, otherwise 50")
    parser.add_argument("--scenario", action="append", choices=("paused", "ignored", "active", "gravity", "frames"))
    args = parser.parse_args()
    if args.runs < 1 or args.duration <= 0:
        parser.error("--runs and --duration must be positive")
    if args.input_rate is not None and args.input_rate <= 0:
        parser.error("--input-rate must be positive")
    for scenario in args.scenario or ("paused", "ignored", "active"):
        rate = args.input_rate or (1000 if scenario == "frames" else 50)
        samples = [measure(args.binary, scenario, args.rts.split(), args.duration, args.level, args.fps, rate) for _ in range(args.runs)]
        medians = {
            key: statistics.median(s[key] for s in samples if s[key] is not None)
            if any(s[key] is not None for s in samples) else None
            for key in samples[0]
        }
        print(json.dumps({"scenario": scenario, "level": args.level, "fps_target": args.fps, "input_rate": rate, "duration_s": args.duration, "median": medians, "samples": samples}), flush=True)


if __name__ == "__main__":
    main()
