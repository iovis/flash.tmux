# flash.tmux (Rust)

A Rust re-implementation of the flash-copy tmux workflow: capture the active pane, open a popup that overlays it, type to filter matches, then press a single label key to copy (and optionally paste) the match.

This keeps the same tmux interaction model as the Python plugin:

- `tmux capture-pane -p -e -J` for visible text
- `tmux display-popup -E -B` to create an overlay popup
- tmux buffers for parent/child IPC
- OSC52 via `tmux set-buffer -w` for clipboard

## Build

```bash
cd /home/david/code/rust/flash.tmux
cargo build --release
```

The tmux entry script will use `target/release/flash-tmux` if present. If not, it falls back to `cargo run --release`.

## Install (TPM style)

Add to `~/.tmux.conf` (path here is local clone):

```bash
run-shell /home/david/code/rust/flash.tmux/flash.tmux
```

Reload:

```bash
tmux source-file ~/.tmux.conf
```

## Usage

- Default binding: `<prefix> S-f` (same as the original plugin with `@flash-copy-bind-key` = `F`).
- Type to search.
- Press the label character to copy.
- Press `;` then a label to auto-paste (when auto-paste is enabled).

## Configuration

All options are read from tmux global options, matching the Python plugin names:

- `@flash-copy-bind-key` (default: `F`)
- `@flash-copy-word-separators` (default: tmux `word-separators`)
- `@flash-copy-case-sensitive` (default: `off`)
- `@flash-copy-reverse-search` (default: `on`)
- `@flash-copy-auto-paste` (default: `on`)
- `@flash-copy-prompt-position` (default: `bottom`)
- `@flash-copy-prompt-indicator` (default: `>`)
- `@flash-copy-prompt-colour` (default: `\033[1m`)
- `@flash-copy-prompt-placeholder-text` (default: `search...`)
- `@flash-copy-highlight-colour` (default: `\033[1;33m`)
- `@flash-copy-label-colour` (default: `\033[1;32m`)
- `@flash-copy-label-characters` (default: built-in label set)
- `@flash-copy-idle-timeout` (default: `15`)
- `@flash-copy-idle-warning` (default: `5`)

Example:

```bash
set -g @flash-copy-bind-key "C-f"
set -g @flash-copy-prompt-indicator ">>"
set -g @flash-copy-auto-paste "off"
run-shell /home/david/code/rust/flash.tmux/flash.tmux
```

## Notes / Differences

- Debug logging is not implemented yet.
- ANSI handling assumes ANSI SGR (`\x1b[...m`) codes for colour resets/highlights.
