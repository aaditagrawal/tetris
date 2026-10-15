# tetris [![Build Status](https://github.com/aaditagrawal/tetris/actions/workflows/ci.yaml/badge.svg)](https://github.com/aaditagrawal/tetris/actions/workflows/ci.yaml)

Terminal Tetris tuned for responsive controls and lower CPU use.
Forked from [samtay/tetris](https://github.com/samtay/tetris).

![Gameplay](./docs/img/play.gif)

## installation

Build this fork from source with [Stack](https://docs.haskellstack.org/en/stable/):

```bash
git clone https://github.com/aaditagrawal/tetris.git
cd tetris
stack install tetris
```

Make sure `~/.local/bin` is on your `PATH`, then run `tetris`.

## usage

```bash
tetris                       # choose a level and progression mode
tetris --level 0             # start immediately at a fixed level
tetris --fps 120             # override the redraw rate
tetris --fps 30              # reduce output on slow SSH connections
tetris --fps 0               # uncapped redraws
tetris --ascii-only          # ASCII ghost preview
tetris --preview-chars 'XX'  # custom ghost preview
tetris --high-score
```

Redraws default to 120 FPS locally and 60 over SSH. Gravity follows the selected
level independently. Hard drop locks and spawns immediately; paused and unchanged
screens sleep. Visible smoothness depends on your terminal and connection.

Move with **←/→/↓** or **h/l/j**, rotate with **↑** or **k**, and hard drop with
**Space**. Use **p** to pause, **r** to restart, and **q** or **Esc** to quit.
Run `tetris --help` for all options.

## development

Use `stack build --fast` while developing and `stack build` for optimized builds.
Keep `~/.stack` and `.stack-work` to reuse compiled dependencies.

```bash
stack test
python3 tools/benchmark.py "$(stack path --local-install-root)/bin/tetris" --scenario frames --fps 120
python3 tools/remote-benchmark.py "$(stack path --local-install-root)/bin/tetris"
```

Benchmarks use isolated terminals and temporary score directories. Frame counts
measure terminal submissions; the remote test simulates a 40 KB/sec output limit.
Neither measures physical display latency or your actual SSH link.
