mod bench_support;

use anyhow::{Context, Result};
use flash_tmux::config::Config;
use flash_tmux::search::{SearchInterface, SearchMatch};

use bench_support::allocation::{CountingAllocator, allocation_counts, reset_activity_counts};

#[global_allocator]
static GLOBAL_ALLOCATOR: CountingAllocator = CountingAllocator;

fn main() -> Result<()> {
    let (input_path, iterations, scenario) = bench_support::parse_args("search_allocation_bench")?;
    let input = std::fs::read_to_string(&input_path)
        .with_context(|| format!("failed to read input: {input_path}"))?;

    let config = Config::defaults();
    let mut search = SearchInterface::new(&input, config.label_characters);
    run_trace(&mut search, scenario.query_steps(), 1);
    let start_live_bytes = reset_activity_counts();

    let checksum = run_trace(&mut search, scenario.query_steps(), iterations);
    let counts = allocation_counts(start_live_bytes);

    println!(
        "checksum={checksum} alloc_calls={} alloc_bytes={} realloc_calls={} realloc_bytes={} dealloc_calls={} start_live_bytes={} end_live_bytes={} peak_live_bytes={}",
        counts.alloc_calls,
        counts.alloc_bytes,
        counts.realloc_calls,
        counts.realloc_bytes,
        counts.dealloc_calls,
        counts.start_live_bytes,
        counts.end_live_bytes,
        counts.peak_live_bytes,
    );
    Ok(())
}

fn run_trace(search: &mut SearchInterface<'_>, query_steps: &[&str], iterations: u64) -> usize {
    let mut checksum = 0usize;
    for _ in 0..iterations {
        for query in query_steps {
            let matches = search.search(query);
            checksum += matches.len();
            if let Some(first) = matches.first() {
                checksum += match_checksum(first);
            }
            checksum += query.len();
        }
    }
    std::hint::black_box(checksum)
}

fn match_checksum(first: &SearchMatch<'_>) -> usize {
    first.line
        + first.col
        + first.match_start
        + first.match_end
        + first.label.map_or(0, ascii_label_checksum)
}

fn ascii_label_checksum(label: char) -> usize {
    let mut buf = [0u8; 4];
    usize::from(label.encode_utf8(&mut buf).as_bytes()[0])
}
