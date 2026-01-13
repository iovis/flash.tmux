# flash.tmux

A tmux plugin inspired by [flash.nvim](https://github.com/folke/flash.nvim) that enables you to search visible strings in the current tmux pane and copy it to system clipboard by pressing the associated label key.

> [!NOTE]
> This is a fork/rewrite of [flash-copy.tmux](https://github.com/Kristijan/flash-copy.tmux)
> Configurations are removed and some behaviors are changed.

## Install

Install with cargo:

```bash
cargo install flash_tmux
```

## tmux setup

Add a binding in `~/.tmux.conf`:

```tmux
bind F run flash_tmux
```

## Usage

- Type to search (ASCII case-insensitive)
- Labels are lowercase
- Lowercase label copies and auto-pastes
- Uppercase label copies only
- Enter or Space selects the first match (auto-paste)
- Exit with Esc, Ctrl-C, or Ctrl-D

## Matching behavior

- Matches are whitespace-delimited tokens.
- If a token is wrapped by `()`, `[]`, `{}`, double quotes, single quotes, or backticks, and the match is inside those wrappers, the outer wrapper is stripped before copying/pasting.
  - Example: `(/home/user/project)` -> `/home/user/project`

## Notes

- Clipboard uses tmux buffers (OSC52 via `set-buffer -w`) with OS fallbacks when available.
- ANSI handling assumes ANSI SGR (`\x1b[...m`) codes for color resets/highlights.
