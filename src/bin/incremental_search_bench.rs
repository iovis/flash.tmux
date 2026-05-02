mod bench_support;

use anyhow::{Context, Result};
use flash_tmux::config::Config;
use flash_tmux::search::{SearchInterface, SearchMatch};

fn main() -> Result<()> {
    let (input_path, iterations, scenario) = bench_support::parse_args("incremental_search_bench")?;
    let input = std::fs::read_to_string(&input_path)
        .with_context(|| format!("failed to read input: {input_path}"))?;

    let config = Config::defaults();
    let mut search = SearchInterface::new(&input, config.label_characters);

    let mut checksum = 0usize;
    for _ in 0..iterations {
        for query in scenario.query_steps() {
            let matches = search.search(query);
            checksum += matches.len();
            if let Some(first) = matches.first() {
                checksum += match_checksum(first);
            }
            checksum += query.len();
        }
    }

    println!("{checksum}");
    Ok(())
}

fn match_checksum(first: &SearchMatch) -> usize {
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
