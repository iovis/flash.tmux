mod bench_support;

use std::hint::black_box;

use anyhow::{Context, Result, bail};
use flash_tmux::config::Config;
use flash_tmux::search::SearchInterface;

use bench_support::cold::{Mode, search_step_checksum, selection_checksum};
use bench_support::render::{RenderScratch, benchmark_render_frame};
use bench_support::{COLD_SELECTION_PREFIXES, Scenario};

fn main() -> Result<()> {
    let (input_path, iterations, scenario, mode) = parse_args()?;
    let input = std::fs::read_to_string(&input_path)
        .with_context(|| format!("failed to read input: {input_path}"))?;
    let config = Config::defaults();

    let mut checksum = 0usize;
    for _ in 0..iterations {
        for prefix in COLD_SELECTION_PREFIXES {
            let mut search = SearchInterface::new_with_trimmable_chars_and_line_capacity(
                &input,
                config.label_characters.clone(),
                config.trimmable_chars.clone(),
                scenario.captured_line_capacity(),
            );
            if mode == Mode::Init {
                checksum = checksum.wrapping_add(search.lines.len());
                black_box(&search);
                continue;
            }

            let mut scratch = RenderScratch::default();
            for depth in 1..=mode.depth() {
                let query = &prefix[..depth];
                checksum = checksum.wrapping_add(run_step(
                    &mut search,
                    query,
                    mode.renders().then_some(&mut scratch),
                    &config,
                    scenario.visible_lines(),
                ));
            }

            if let Some(depth) = mode.backspace_depth() {
                checksum = checksum.wrapping_add(run_step(
                    &mut search,
                    &prefix[..depth],
                    None,
                    &config,
                    scenario.visible_lines(),
                ));
            }

            checksum = checksum.wrapping_add(selection_checksum(&search, scenario.visible_lines()));
        }
    }

    println!("{checksum}");
    Ok(())
}

fn run_step(
    search: &mut SearchInterface<'_>,
    query: &str,
    scratch: Option<&mut RenderScratch>,
    config: &Config,
    visible_lines: usize,
) -> usize {
    let mut checksum = search_step_checksum(search, query);
    if let Some(scratch) = scratch {
        checksum = checksum.wrapping_add(benchmark_render_frame(
            search,
            query,
            scratch,
            config,
            visible_lines,
        ));
    }
    checksum
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
        Scenario::parse(Some(&scenario), "cold_selection_bench")?,
        Mode::parse(&mode)?,
    ))
}

fn usage<T>() -> Result<T> {
    bail!(
        "usage: cold_selection_bench <input-path> <iterations> <shell-sparse|development|copy-mode> <init|search-1|search-2|search-3|backspace-2|backspace-3|render-1|render-2|render-3>"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cold_modes_have_expected_shapes() {
        assert_eq!(Mode::parse("init").unwrap().depth(), 0);
        assert_eq!(Mode::parse("search-1").unwrap().depth(), 1);
        assert_eq!(Mode::parse("render-2").unwrap().depth(), 2);
        assert_eq!(Mode::parse("search-3").unwrap().depth(), 3);
        assert_eq!(
            Mode::parse("backspace-2").unwrap().backspace_depth(),
            Some(1)
        );
        assert_eq!(
            Mode::parse("backspace-3").unwrap().backspace_depth(),
            Some(2)
        );
        assert!(Mode::parse("render-3").unwrap().renders());
        assert!(Mode::parse("unknown").is_err());
    }

    #[test]
    fn cold_prefixes_are_three_ascii_bytes() {
        assert!(
            COLD_SELECTION_PREFIXES
                .iter()
                .all(|prefix| prefix.len() == 3 && prefix.is_ascii())
        );
    }

    #[test]
    fn cold_prefixes_select_in_every_realistic_fixture() {
        let fixtures = [
            (
                include_str!("../../bench/realistic/fixtures/shell-sparse-97x52.txt"),
                Scenario::ShellSparse,
            ),
            (
                include_str!("../../bench/realistic/fixtures/development-97x52.txt"),
                Scenario::Development,
            ),
            (
                include_str!("../../bench/realistic/fixtures/copy-mode-195x52.txt"),
                Scenario::CopyMode,
            ),
        ];
        let config = Config::defaults();

        for (fixture, scenario) in fixtures {
            assert_eq!(
                scenario.captured_line_capacity(),
                fixture.split('\n').count()
            );
            for prefix in COLD_SELECTION_PREFIXES {
                for depth in 1..=3 {
                    let mut search = SearchInterface::new_with_trimmable_chars_and_line_capacity(
                        fixture,
                        config.label_characters.clone(),
                        config.trimmable_chars.clone(),
                        scenario.captured_line_capacity(),
                    );
                    for query_depth in 1..=depth {
                        search.search(&prefix[..query_depth]);
                    }
                    selection_checksum(&search, scenario.visible_lines());
                }
            }
        }
    }
}
