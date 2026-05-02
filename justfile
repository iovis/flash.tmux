# list recipes
default:
    @just --list

fixture := "../flash-bench-content.txt"
realistic_dir := "bench/realistic/fixtures"

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

checksums-realistic: release
    ./target/release/search_bench "{{ realistic_dir }}/shell-sparse-97x52.txt" 1 shell-sparse
    ./target/release/incremental_search_bench "{{ realistic_dir }}/shell-sparse-97x52.txt" 1 shell-sparse
    ./target/release/render_incremental_search_bench "{{ realistic_dir }}/shell-sparse-97x52.txt" 1 shell-sparse
    ./target/release/search_bench "{{ realistic_dir }}/development-97x52.txt" 1 development
    ./target/release/incremental_search_bench "{{ realistic_dir }}/development-97x52.txt" 1 development
    ./target/release/render_incremental_search_bench "{{ realistic_dir }}/development-97x52.txt" 1 development
    ./target/release/search_bench "{{ realistic_dir }}/copy-mode-195x52.txt" 1 copy-mode
    ./target/release/incremental_search_bench "{{ realistic_dir }}/copy-mode-195x52.txt" 1 copy-mode
    ./target/release/render_incremental_search_bench "{{ realistic_dir }}/copy-mode-195x52.txt" 1 copy-mode

checksums-cold: release
    #!/usr/bin/env bash
    set -euo pipefail
    for scenario in shell-sparse development copy-mode; do
      case "$scenario" in
        shell-sparse) fixture="{{ realistic_dir }}/shell-sparse-97x52.txt" ;;
        development) fixture="{{ realistic_dir }}/development-97x52.txt" ;;
        copy-mode) fixture="{{ realistic_dir }}/copy-mode-195x52.txt" ;;
      esac
      for mode in init search-1 search-2 search-3 backspace-2 backspace-3 render-1 render-2 render-3; do
        printf '%-13s %-18s ' "$scenario" "$mode"
        ./target/release/cold_selection_bench "$fixture" 1 "$scenario" "$mode"
      done
    done

checksums-all: checksums checksums-realistic checksums-cold

measure-realistic-allocations iterations="1": release
    ./target/release/search_allocation_bench "{{ realistic_dir }}/shell-sparse-97x52.txt" "{{ iterations }}" shell-sparse
    ./target/release/search_allocation_bench "{{ realistic_dir }}/development-97x52.txt" "{{ iterations }}" development
    ./target/release/search_allocation_bench "{{ realistic_dir }}/copy-mode-195x52.txt" "{{ iterations }}" copy-mode

measure-cold-allocations iterations="1": release
    #!/usr/bin/env bash
    set -euo pipefail
    for scenario in shell-sparse development copy-mode; do
      case "$scenario" in
        shell-sparse) fixture="{{ realistic_dir }}/shell-sparse-97x52.txt" ;;
        development) fixture="{{ realistic_dir }}/development-97x52.txt" ;;
        copy-mode) fixture="{{ realistic_dir }}/copy-mode-195x52.txt" ;;
      esac
      for mode in init search-1 search-2 search-3 backspace-2 backspace-3; do
        printf '%-13s %-12s ' "$scenario" "$mode"
        ./target/release/cold_selection_allocation_bench "$fixture" "{{ iterations }}" "$scenario" "$mode"
      done
    done

bench: bench-search bench-incremental bench-render

bench-all: bench bench-realistic

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

bench-realistic: bench-realistic-search bench-realistic-incremental bench-realistic-render

bench-realistic-search iterations="6000" runs="10": release
    hyperfine --warmup 3 --runs "{{ runs }}" \
      --command-name sparse "./target/release/search_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse" \
      --command-name development "./target/release/search_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development" \
      --command-name copy-mode "./target/release/search_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode"

bench-realistic-incremental iterations="3000" runs="10": release
    hyperfine --warmup 3 --runs "{{ runs }}" \
      --command-name sparse "./target/release/incremental_search_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse" \
      --command-name development "./target/release/incremental_search_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development" \
      --command-name copy-mode "./target/release/incremental_search_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode"

bench-realistic-render iterations="1000" runs="10": release
    hyperfine --warmup 3 --runs "{{ runs }}" \
      --command-name sparse "./target/release/render_incremental_search_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse" \
      --command-name development "./target/release/render_incremental_search_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development" \
      --command-name copy-mode "./target/release/render_incremental_search_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode"

bench-cold: bench-cold-search bench-cold-render bench-cold-backspace

bench-cold-search iterations="10000" runs="10": release
    hyperfine --warmup 3 --runs "{{ runs }}" \
      --command-name sparse-init "./target/release/cold_selection_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse init" \
      --command-name sparse-1 "./target/release/cold_selection_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse search-1" \
      --command-name sparse-2 "./target/release/cold_selection_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse search-2" \
      --command-name sparse-3 "./target/release/cold_selection_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse search-3" \
      --command-name development-init "./target/release/cold_selection_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development init" \
      --command-name development-1 "./target/release/cold_selection_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development search-1" \
      --command-name development-2 "./target/release/cold_selection_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development search-2" \
      --command-name development-3 "./target/release/cold_selection_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development search-3" \
      --command-name copy-mode-init "./target/release/cold_selection_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode init" \
      --command-name copy-mode-1 "./target/release/cold_selection_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode search-1" \
      --command-name copy-mode-2 "./target/release/cold_selection_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode search-2" \
      --command-name copy-mode-3 "./target/release/cold_selection_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode search-3"

bench-cold-render iterations="1000" runs="10": release
    hyperfine --warmup 3 --runs "{{ runs }}" \
      --command-name sparse-1 "./target/release/cold_selection_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse render-1" \
      --command-name sparse-2 "./target/release/cold_selection_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse render-2" \
      --command-name sparse-3 "./target/release/cold_selection_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse render-3" \
      --command-name development-1 "./target/release/cold_selection_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development render-1" \
      --command-name development-2 "./target/release/cold_selection_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development render-2" \
      --command-name development-3 "./target/release/cold_selection_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development render-3" \
      --command-name copy-mode-1 "./target/release/cold_selection_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode render-1" \
      --command-name copy-mode-2 "./target/release/cold_selection_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode render-2" \
      --command-name copy-mode-3 "./target/release/cold_selection_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode render-3"

bench-cold-backspace iterations="10000" runs="10": release
    hyperfine --warmup 3 --runs "{{ runs }}" \
      --command-name sparse-forward-2 "./target/release/cold_selection_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse search-2" \
      --command-name sparse-backspace-2 "./target/release/cold_selection_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse backspace-2" \
      --command-name sparse-forward-3 "./target/release/cold_selection_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse search-3" \
      --command-name sparse-backspace-3 "./target/release/cold_selection_bench {{ realistic_dir }}/shell-sparse-97x52.txt {{ iterations }} shell-sparse backspace-3" \
      --command-name development-forward-2 "./target/release/cold_selection_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development search-2" \
      --command-name development-backspace-2 "./target/release/cold_selection_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development backspace-2" \
      --command-name development-forward-3 "./target/release/cold_selection_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development search-3" \
      --command-name development-backspace-3 "./target/release/cold_selection_bench {{ realistic_dir }}/development-97x52.txt {{ iterations }} development backspace-3" \
      --command-name copy-mode-forward-2 "./target/release/cold_selection_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode search-2" \
      --command-name copy-mode-backspace-2 "./target/release/cold_selection_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode backspace-2" \
      --command-name copy-mode-forward-3 "./target/release/cold_selection_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode search-3" \
      --command-name copy-mode-backspace-3 "./target/release/cold_selection_bench {{ realistic_dir }}/copy-mode-195x52.txt {{ iterations }} copy-mode backspace-3"

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
