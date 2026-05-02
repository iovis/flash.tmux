# AGENTS

Context for this repo:

- Rust CLI for tmux “flash copy” workflow (inspired by flash.nvim / flash-copy.tmux).
- Project path: `/home/david/code/rust/flash.tmux`.
- Now a standalone CLI (not a tmux plugin).

## tmux workflow and behavior

- Pane capture (no ANSI): `tmux capture-pane -p -J`.
- Popup: `tmux display-popup -E -B`.
- Parent/child IPC via tmux buffers (`set-buffer`, `show-buffer`, `delete-buffer`).
- Clipboard primarily via `tmux set-buffer -w` (OSC52), with fallback to `pbcopy`/`xclip`/`xsel`.
- Auto-paste uses tmux `paste-buffer` into the target pane.
- Enter pastes and then sends Enter; Space pastes and then sends Space.

## Input / matching behavior

- Search tokens are whitespace-delimited; match is case-insensitive.
- Labels are lowercase only; typing lowercase label auto-pastes, uppercase copies only.
- Enter/Space pick the first match; Ctrl-C/Ctrl-D/Esc cancel.
- Output text trims leading/trailing wrapper chars and punctuation ((), [], {}, quotes, backticks, commas, periods, colons, semicolons) but never trims through the match span.

## UI styling

- Base text is always dimmed.
- Truecolor highlight palette:
  - match: `#babbf2`
  - current: `#ef9f77`
  - label: `#a6d18a` (bold)
- Prompt indicator uses terminal “purple” (`Color::Magenta`) and is bold.

## Architecture

- `src/main.rs`: CLI + parent/interactive orchestration.
- `src/ui.rs`: input loop + rendering.
- `src/search.rs`: matching, labels, trimming helpers.
- `src/tmux.rs`: tmux helpers + clipboard.
- `src/config.rs`: styling/config defaults.
- `src/lib.rs`: module declarations.
- `src/bin/*_bench.rs`: standalone benchmark binaries adapted to the v0.2.2
  owned-match API.

## Benchmarks

- This historical benchmark branch keeps the product implementation pinned to
  `v0.2.2` while tracking the benchmark contract from `bench`.
- Scaled synthetic fixture: `../flash-bench-content.txt`.
- Actual-size realistic fixtures and their dimensions/checksums are documented
  in `bench/realistic/README.md`.
- Build release app and benchmark binaries with `just release`.
- Verify benchmark checksums with `just checksums-all`.
  - Expected synthetic outputs: `1787`, `4278`, `6728401868494091241`.
- Run the scaled synthetic suite with `just bench`.
- Run the actual-size suite with `just bench-realistic`.
- Run cold-selection initialization, search, backspace, and render workloads
  with `just bench-cold`.
- `just bench-all` runs the synthetic and realistic suites. Keep the realistic
  workloads primary and the synthetic suite as a historical throughput gate.
- `just measure-realistic-allocations` and `just measure-cold-allocations`
  report allocation behavior for the historical implementation.
- Benchmark recipes accept optional iteration and run counts; synthetic recipes
  additionally accept a fixture copy count.
- `hyperfine` must be installed to run benchmark recipes.
- If `just` cannot write runtime files under `/run/user/...`, run with
  `XDG_RUNTIME_DIR=/tmp`.

## Rust conventions

- Prefer `pub` (avoid `pub(crate)`).
- Keep changes simple; avoid overengineering.
- Code quality commands:
  - `cargo fmt`
  - `cargo clippy -- -W clippy::pedantic -A clippy::missing-errors-doc -A clippy::missing-panics-doc -A clippy::must-use-candidate`
  - `cargo test`
