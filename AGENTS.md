# AGENTS

Context for this repo:

- Rust CLI for tmux “flash copy” workflow (inspired by flash.nvim / flash-copy.tmux).
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
- Matches that resolve to the same case-sensitive text share a label.
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
- `src/bin/*_bench.rs`: standalone benchmark binaries for cross-implementation comparisons. (only in `bench` branch)

## Benchmarks

- Only in `bench` branch.
- Scaled synthetic fixture: `../flash-bench-content.txt`.
- Actual-size realistic fixtures and their dimensions/checksums are documented
  in `bench/realistic/README.md`.
- Build release app and benchmark binaries with `just release`.
- Verify benchmark checksum parity with `just checksums`.
  - Expected outputs: `1787`, `4278`, `5838609766282676217`.
- Run Rust-only hyperfine benchmarks with:
  - `just bench-search`
  - `just bench-incremental`
  - `just bench-render`
  - `just bench`
- Run the separate realistic suite with:
  - `just checksums-realistic`
  - `just bench-realistic-search`
  - `just bench-realistic-incremental`
  - `just bench-realistic-render`
  - `just bench-realistic`
- `just checksums-all` and `just bench-all` run both suites.
- `just measure-realistic-allocations` reports search allocations after one
  warmup edit trace; pass an iteration count to measure repeated traces.
- Benchmark recipes accept optional `copies`, `iterations`, and `runs`
  parameters, matching the C and Odin sibling repos.
- Realistic recipes accept optional `iterations` and `runs` parameters. They
  repeat query traces in process but never multiply fixture contents.
- Use `just bench-all` for optimization decisions. Treat the three realistic
  scenarios as the primary profiling workloads and retain the synthetic suite
  as a historical throughput and checksum regression gate.
- `hyperfine` must be installed to run benchmark recipes.
- If `just` cannot write its runtime temp files under `/run/user/...`, run with `XDG_RUNTIME_DIR=/tmp`.

## Rust conventions

- Prefer `pub` (avoid `pub(crate)`).
- Keep changes simple; avoid overengineering.
- Code quality commands:
  - `cargo fmt`
  - `cargo clippy -- -W clippy::pedantic -A clippy::missing-errors-doc -A clippy::missing-panics-doc -A clippy::must-use-candidate`
  - `cargo test`
