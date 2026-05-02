use anyhow::{Context, Result, bail};
use flash_tmux::config::Config;
use flash_tmux::search::{SearchInterface, SearchMatch};

const QUERIES: &[&str] = &[
    "b",
    "be",
    "ben",
    "bench",
    "benchmark",
    "target",
    "src",
    "rs",
    "guide",
    "workspace",
    "relative",
    "omega",
    "tmp",
    "zzz",
];

fn main() -> Result<()> {
    let (input_path, iterations) = parse_args()?;
    let input = std::fs::read_to_string(&input_path)
        .with_context(|| format!("failed to read input: {input_path}"))?;

    let config = Config::defaults();
    let mut search = SearchInterface::new(&input, config.label_characters);

    let mut checksum = 0usize;
    for _ in 0..iterations {
        for query in QUERIES {
            let matches = search.search(query);
            checksum += matches.len();
            if let Some(first) = matches.first() {
                checksum += match_checksum(first);
            }
        }
    }

    println!("{checksum}");
    Ok(())
}

fn parse_args() -> Result<(String, u64)> {
    let mut args = std::env::args().skip(1);
    let Some(input_path) = args.next() else {
        bail!("usage: search_bench <input-path> <iterations>");
    };
    let Some(iterations) = args.next() else {
        bail!("usage: search_bench <input-path> <iterations>");
    };
    if args.next().is_some() {
        bail!("usage: search_bench <input-path> <iterations>");
    }

    let iterations = iterations
        .parse::<u64>()
        .with_context(|| format!("invalid iterations: {iterations}"))?;
    if iterations == 0 {
        bail!("invalid iterations: {iterations}");
    }

    Ok((input_path, iterations))
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
