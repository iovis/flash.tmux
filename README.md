# flash.tmux

Standalone Rust CLI for a tmux "flash copy" workflow inspired by
[flash.nvim](https://github.com/folke/flash.nvim) and
[flash-copy.tmux](https://github.com/Kristijan/flash-copy.tmux).

Quickly copy text from a tmux pane by searching and selecting it with
labeled keys.

## Requirements

- tmux with `display-popup` support.
- Clipboard support via tmux (OSC52) or one of: `pbcopy`, `xclip`, `xsel`.

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
- Enter: paste and send Enter.
- Space: paste and send Space.
- Cancel with Esc, Ctrl-C, or Ctrl-D.
- Edit query with Backspace, Ctrl-U (clear), Ctrl-W (delete word).

## Matching behavior

- Only the visible pane content can be matched.
- Matches are whitespace-delimited tokens.
- Substring matches within a token are allowed.
- If a token is wrapped by `()`, `[]`, `{}`, quotes, or backticks, and the
  match is inside those wrappers, the outer wrapper is stripped before
  copying/pasting.
  - Example: `(/home/user/project)` → `/home/user/project`
