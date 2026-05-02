mod bench_support;

use std::hint::black_box;

use anyhow::{Context, Result, bail};
use flash_tmux::config::Config;
use flash_tmux::search::SearchInterface;

use bench_support::allocation::{CountingAllocator, allocation_counts, reset_activity_counts};
use bench_support::cold::{Mode, search_step_checksum, selection_checksum};
use bench_support::{COLD_SELECTION_PREFIXES, Scenario};

#[global_allocator]
static GLOBAL_ALLOCATOR: CountingAllocator = CountingAllocator;

fn main() -> Result<()> {
    let (input_path, iterations, scenario, mode) = parse_args()?;
    if mode.renders() {
        bail!("cold allocation modes do not render");
    }

    let input = std::fs::read_to_string(&input_path)
        .with_context(|| format!("failed to read input: {input_path}"))?;
    let config = Config::defaults();

    run_workload(&input, &config, scenario, mode, 1);
    let start_live_bytes = reset_activity_counts();
    let checksum = run_workload(&input, &config, scenario, mode, iterations);
    let counts = allocation_counts(start_live_bytes);
    let selections = usize::try_from(iterations)
        .expect("iteration count must fit usize")
        .wrapping_mul(COLD_SELECTION_PREFIXES.len());
    let peak_extra_bytes = counts
        .peak_live_bytes
        .saturating_sub(counts.start_live_bytes);
    let (alloc_calls_whole, alloc_calls_tenth) =
        per_selection_tenths(counts.alloc_calls, selections);
    let (alloc_bytes_whole, alloc_bytes_tenth) =
        per_selection_tenths(counts.alloc_bytes, selections);

    println!(
        "checksum={checksum} selections={selections} alloc_calls={} alloc_bytes={} realloc_calls={} realloc_bytes={} dealloc_calls={} start_live_bytes={} end_live_bytes={} peak_extra_bytes={} alloc_calls_per_selection={alloc_calls_whole}.{alloc_calls_tenth} alloc_bytes_per_selection={alloc_bytes_whole}.{alloc_bytes_tenth}",
        counts.alloc_calls,
        counts.alloc_bytes,
        counts.realloc_calls,
        counts.realloc_bytes,
        counts.dealloc_calls,
        counts.start_live_bytes,
        counts.end_live_bytes,
        peak_extra_bytes,
    );
    Ok(())
}

fn per_selection_tenths(total: usize, selections: usize) -> (usize, usize) {
    let whole = total / selections;
    let tenth = (total % selections) * 10 / selections;
    (whole, tenth)
}

fn run_workload(
    input: &str,
    config: &Config,
    scenario: Scenario,
    mode: Mode,
    iterations: u64,
) -> usize {
    let mut checksum = 0usize;
    for _ in 0..iterations {
        for prefix in COLD_SELECTION_PREFIXES {
            let mut search = SearchInterface::new(input, config.label_characters.clone());
            if mode == Mode::Init {
                checksum = checksum.wrapping_add(search.lines.len());
                black_box(&search);
                continue;
            }

            for depth in 1..=mode.depth() {
                checksum =
                    checksum.wrapping_add(search_step_checksum(&mut search, &prefix[..depth]));
            }
            if let Some(depth) = mode.backspace_depth() {
                checksum =
                    checksum.wrapping_add(search_step_checksum(&mut search, &prefix[..depth]));
            }
            checksum = checksum.wrapping_add(selection_checksum(&search, scenario.visible_lines()));
        }
    }
    black_box(checksum)
}

fn parse_args() -> Result<(String, u64, Scenario, Mode)> {
    let mut args = std::env::args().skip(1);
    let Some(input_path) = args.next() else {
        return usage();
    };
    let Some(iterations) = args.next() else {
        return usage();
    };
    let Some(scenario) = args.next() else {
        return usage();
    };
    let Some(mode) = args.next() else {
        return usage();
    };
    if args.next().is_some() {
        return usage();
    }

    let iterations = iterations
        .parse::<u64>()
        .with_context(|| format!("invalid iterations: {iterations}"))?;
    if iterations == 0 {
        bail!("invalid iterations: {iterations}");
    }

    Ok((
        input_path,
        iterations,
        Scenario::parse(Some(&scenario), "cold_selection_allocation_bench")?,
        Mode::parse(&mode)?,
    ))
}

fn usage<T>() -> Result<T> {
    bail!(
        "usage: cold_selection_allocation_bench <input-path> <iterations> <shell-sparse|development|copy-mode> <init|search-1|search-2|search-3|backspace-2|backspace-3>"
    )
}
