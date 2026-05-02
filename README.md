# flash.tmux

Standalone Rust CLI for a tmux "flash copy" workflow inspired by
[flash.nvim](https://github.com/folke/flash.nvim) and
[flash-copy.tmux](https://github.com/Kristijan/flash-copy.tmux).

Quickly copy text from a tmux pane by searching and selecting it with
labeled keys.

## Requirements

- tmux with `display-popup` support.
- Clipboard support via tmux `set-buffer -w` (OSC52-capable terminal).

## Install

From crates.io:

```bash
cargo install flash_tmux
```

From source:

```bash
git clone https://github.com/iovis/flash.tmux
cd flash.tmux
cargo install --path .
```

## tmux setup

Add a binding in `~/.tmux.conf`:

```tmux
bind-key F run-shell "flash_tmux"
```

## Usage

- Type to search (ASCII case-insensitive).
- Labels are lowercase; press a label to select.
  - Lowercase: copy + paste.
  - Uppercase: copy only.
- Tab: paste current match
- Enter: paste and send Enter.
- Space: paste and send Space.

### Options

- `-r`, `--reverse-label`: Reverse label behavior.
  - Lowercase: copy only.
  - Uppercase: copy + paste.

## Matching behavior

- Only the visible pane content can be matched.
- Matches are whitespace-delimited tokens.
- Substring matches within a token are allowed.
- If a token is wrapped by `()`, `[]`, `{}`, quotes, or backticks, and the
  match is inside those wrappers, the outer wrapper is stripped before
  copying/pasting.
  - Example: `(/home/user/project)` → `/home/user/project`

## Benchmarks

Standalone benchmark binaries live in `src/bin/` (on the `bench` branch) and
are intended for comparison with the C and Go sibling implementations.

Build release binaries and verify checksum parity:

```bash
just release
just checksums
```

Expected checksum output:

```text
1787
4278
6728401868494091241
```

Run Rust-only benchmarks with `hyperfine`:

```bash
just bench-search
just bench-incremental
just bench-render
just bench
```

The benchmark recipes use `../flash-bench-content.txt` by default and accept
optional `copies`, `iterations`, and `runs` parameters. For example:

```bash
just bench-search 500 50 10
```
