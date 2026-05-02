mod bench_support;

use anyhow::{Context, Result};
use flash_tmux::config::Config;
use flash_tmux::search::SearchInterface;

use bench_support::render::{RenderScratch, benchmark_render_frame};

fn main() -> Result<()> {
    let (input_path, iterations, scenario) =
        bench_support::parse_args("render_incremental_search_bench")?;
    let input = std::fs::read_to_string(&input_path)
        .with_context(|| format!("failed to read input: {input_path}"))?;

    let config = Config::defaults();
    let mut search = SearchInterface::new(&input, config.label_characters.clone());
    let mut scratch = RenderScratch::default();

    let mut checksum = 0usize;
    for _ in 0..iterations {
        for query in scenario.query_steps() {
            search.search(query);
            checksum += benchmark_render_frame(
                &search,
                query,
                &mut scratch,
                &config,
                scenario.visible_lines(),
            );
        }
    }

    println!("{checksum}");
    Ok(())
}
