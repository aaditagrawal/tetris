# tetris [![Build Status](https://github.com/samtay/tetris/actions/workflows/ci.yaml/badge.svg)](https://github.com/samtay/tetris/actions/workflows/ci.yaml)

A terminal interface for Tetris

![terminal-gif](./docs/img/play.gif)

## installation
Installation on MacOS and Linux is outlined below. Windows support is questionable, but you can try to install from [source](#install-from-source).

#### MacOS
Installation on a Mac is simple with Homebrew:
```bash
brew install samtay/tui/tetris
```
#### Arch Linux
Arch Linux users can install from the [AUR](https://aur.archlinux.org/packages/tetris-terminal-git/), e.g.
```bash
yay -S tetris-terminal-git # or yaourt, etc.
```
#### Snapcraft
Thanks to **@thefenriswolf** this is available on most Linux distributions via [snapcraft](https://snapcraft.io/tetris-thefenriswolf):
```bash
sudo snap install tetris-thefenriswolf
alias tetris=/snap/bin/tetris-thefenriswolf.tetris # add to .bashrc or .zshrc etc.
```
#### install from source
First [get stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). Then
```bash
git clone https://github.com/samtay/tetris.git
cd tetris
stack install tetris
```

## usage

The default game is run by simply executing the `tetris` command.
If the unicode characters look a bit
wonky in your terminal, you can also run
```shell
tetris --ascii-only         # uses [] as preview cell
# or
tetris --preview-chars 'XX' # uses custom characters as preview cell
```
If you want to skip the level prompt, you can start the game immediately via
```shell
tetris --level n
```
Redraws default to 120 FPS locally and 60 over SSH. Override with `--fps N`
(0 is uncapped); try `--fps 30` on slow connections. Gravity follows the chosen
level independently. Paused and unchanged screens sleep.

Lastly, to see the current high score, you can run `tetris --high-score`.
And of course, see `tetris --help` for help.

## development

Use `stack build --fast` during development and `stack build` for optimized
builds. Keep `~/.stack` and `.stack-work` to reuse compiled dependencies.

```bash
stack build
stack test
python3 tools/benchmark.py "$(stack path --local-install-root)/bin/tetris" --scenario frames --fps 120
python3 tools/remote-benchmark.py "$(stack path --local-install-root)/bin/tetris"
```

Benchmarks use isolated terminals and temporary score directories. Frame counts
measure terminal submissions; the remote test simulates a 40 KB/sec output limit.
Neither measures physical display latency or your actual SSH link.

Local verification sustained about 120 redraws/sec through 100 cleared lines;
engine regression checks cover 500 clears. The reported SSH slowdown remains
unconfirmed on the actual connection.

## tips

#### troubleshooting
People seem to have varying levels of success with the linux binary. Please note that it is compiled dynamically and hence should not be expected to work on most distros. If you have other problems, feel free to open an issue.

#### roll your own
If you like games in your terminal and have an interest in functional programming, write your own! This code is built on top of [brick](https://github.com/jtdaugherty/brick) which makes building terminal user interfaces very accessible. I also have a [tutorial](https://samtay.github.io/posts/introduction-to-brick) that can help you get started.
