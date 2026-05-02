# list recipes
default:
    @just --list

fixture := "../flash-bench-content.txt"

# Run tests with nextest
@test:
    cargo nextest run

# Build release binaries, including benchmark binaries under src/bin.
release:
    cargo build --release --bins

checksums: release
    ./target/release/search_bench "{{ fixture }}" 1
    ./target/release/incremental_search_bench "{{ fixture }}" 1
    ./target/release/render_incremental_search_bench "{{ fixture }}" 1

bench: bench-search bench-incremental bench-render

bench-search copies="500" iterations="50" runs="5": release
    #!/usr/bin/env bash
    set -euo pipefail
    input=$(mktemp --suffix=.txt)
    trap 'rm -f "$input"' EXIT
    for _ in $(seq 1 "{{ copies }}"); do
      cat "{{ fixture }}" >> "$input"
    done
    hyperfine --warmup 2 --runs "{{ runs }}" --command-name rust "./target/release/search_bench $input {{ iterations }}"

bench-incremental copies="500" iterations="30" runs="5": release
    #!/usr/bin/env bash
    set -euo pipefail
    input=$(mktemp --suffix=.txt)
    trap 'rm -f "$input"' EXIT
    for _ in $(seq 1 "{{ copies }}"); do
      cat "{{ fixture }}" >> "$input"
    done
    hyperfine --warmup 2 --runs "{{ runs }}" --command-name rust "./target/release/incremental_search_bench $input {{ iterations }}"

bench-render copies="300" iterations="20" runs="5": release
    #!/usr/bin/env bash
    set -euo pipefail
    input=$(mktemp --suffix=.txt)
    trap 'rm -f "$input"' EXIT
    for _ in $(seq 1 "{{ copies }}"); do
      cat "{{ fixture }}" >> "$input"
    done
    hyperfine --warmup 2 --runs "{{ runs }}" --command-name rust "./target/release/render_incremental_search_bench $input {{ iterations }}"

# Install locally
@install:
    cargo install --path .

# Compile and open docs
@docs:
    cargo doc --open

# Open project in Github
open:
    gh repo view --web

# Open an evcxr console
console:
    # Use `:dep .` to load current crate
    @evcxr

# Upgrade dependencies
upgrade:
    cargo upgrade --incompatible allow
