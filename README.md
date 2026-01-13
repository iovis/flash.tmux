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
- Type a lowercase label to copy + auto-paste, or an uppercase label to copy only.
- Exit with Esc, Ctrl-C, or Ctrl-D.

## Configuration

Only the tmux key binding is configurable:

- `@flash-copy-bind-key` (default: `F`)

Example:

```bash
set -g @flash-copy-bind-key "C-f"
run-shell /home/david/code/rust/flash.tmux/flash.tmux
```

## Notes / Differences

- Debug logging is not implemented yet.
- ANSI handling assumes ANSI SGR (`\x1b[...m`) codes for color resets/highlights.
