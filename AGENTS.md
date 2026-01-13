# AGENTS

Context for this repo:

- This is a Rust re-implementation of the tmux flash-copy workflow.
- Original reference plugin (Python): `/home/david/.local/share/muxi/plugins/flash-copy.tmux`
- Current project path: `/home/david/code/rust/flash.tmux`
- Core tmux workflow to keep in mind:
  - `tmux capture-pane -p -e -J` to capture visible text
  - `tmux display-popup -E -B` to overlay a popup over the active pane
  - tmux buffers for parent/child IPC (`set-buffer`, `show-buffer`, `delete-buffer`)
  - clipboard via `tmux set-buffer -w` (OSC52), with `paste-buffer` for auto-paste

Notes from prior discussion:

- The black "flash" on invocation is primarily the popup opening + screen clear, not interpreter startup.
- The Rust implementation mirrors the same tmux-level behavior and config names as the Python plugin.
