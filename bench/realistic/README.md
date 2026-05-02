# Realistic benchmark scenarios for v0.2.2

These fixtures model one tmux viewport without repeating its contents. They
complement the scaled synthetic benchmark in `../flash-bench-content.txt`:

| Scenario | Intended viewport | Bytes | Lines | Tokens | Longest token |
| --- | ---: | ---: | ---: | ---: | ---: |
| `shell-sparse` | 97 x 52 | 784 | 52 | 90 | 39 bytes |
| `development` | 97 x 52 | 3,315 | 52 | 265 | 107 bytes |
| `copy-mode` | 195 x 52 | 2,816 | 52 | 189 | 162 bytes |

The fixtures are synthetic and sanitized, but preserve terminal-like prompts,
blank rows, paths, diagnostics, repeated selections, punctuation, UTF-8, and
joined long lines. They are not a claim about a universal average pane. Keeping
the scenarios separate makes sparse-pane regressions and long-token wins
visible instead of averaging them together.

The plain-search benchmark resets the query before every realistic query, so it
measures full scans. Incremental search and rendering replay a persistent edit
trace containing appends, backspaces, resets, a correction, common hits, paths,
and misses. Realistic rendering covers 51 content rows, leaving one row for the
prompt as the interactive UI does.

Run `just checksums-realistic` and expect the values below in scenario order:

| Scenario | Search | Incremental | Render |
| --- | ---: | ---: | ---: |
| `shell-sparse` | 1,217 | 4,154 | 1,389,734,728,545,542,118 |
| `development` | 2,403 | 9,815 | 2,329,724,019,247,655,900 |
| `copy-mode` | 2,327 | 9,479 | 15,355,434,905,799,317,228 |

Search and incremental checksums match the current implementation. Render
checksums are specific to v0.2.2 because it assigns labels per match rather
than sharing a label between matches with the same selected text.

Run all three timing workloads with `just bench-realistic`. Unlike the
synthetic recipes, these recipes increase only the in-process trace count; they
never concatenate fixture copies.

Run `just measure-realistic-allocations` to count allocations during one edit
trace after construction and a warmup trace. The benchmark also reports live
bytes before and after the measured trace and its peak live-byte count. Pass an
iteration count to measure repeated traces, for example
`just measure-realistic-allocations 100`.
