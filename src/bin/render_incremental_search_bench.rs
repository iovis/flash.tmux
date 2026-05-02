use anyhow::{Context, Result, bail};
use flash_tmux::config::Config;
use flash_tmux::search::{SearchInterface, SearchMatch};

const QUERY_STEPS: &[&str] = &[
    "b", "be", "ben", "be", "ben", "benc", "bench", "ben", "be", "", "s", "sr", "src", "sr", "s",
    "", "t", "ta", "tar", "targ", "target", "targe", "target", "", "o", "om", "ome", "omeg",
    "omega", "ome", "omega", "", "z", "zz", "zzz", "zz", "",
];
const VISIBLE_LINES: usize = 20;

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
enum StyleKind {
    Base,
    Highlight,
    Current,
}

#[derive(Clone, Copy)]
struct LabelPosition {
    pos: usize,
    label: char,
}

#[derive(Default)]
struct RenderScratch {
    labels: Vec<LabelPosition>,
    style_map: Vec<StyleKind>,
}

fn main() -> Result<()> {
    let (input_path, iterations) = parse_args()?;
    let input = std::fs::read_to_string(&input_path)
        .with_context(|| format!("failed to read input: {input_path}"))?;

    let config = Config::defaults();
    let mut search = SearchInterface::new(&input, config.label_characters.clone());
    let mut scratch = RenderScratch::default();

    let mut checksum = 0usize;
    for _ in 0..iterations {
        for query in QUERY_STEPS {
            search.search(query);
            checksum += benchmark_render_frame(&search, query, &mut scratch, &config);
        }
    }

    println!("{checksum}");
    Ok(())
}

fn parse_args() -> Result<(String, u64)> {
    let mut args = std::env::args().skip(1);
    let Some(input_path) = args.next() else {
        bail!("usage: render_incremental_search_bench <input-path> <iterations>");
    };
    let Some(iterations) = args.next() else {
        bail!("usage: render_incremental_search_bench <input-path> <iterations>");
    };
    if args.next().is_some() {
        bail!("usage: render_incremental_search_bench <input-path> <iterations>");
    }

    let iterations = iterations
        .parse::<u64>()
        .with_context(|| format!("invalid iterations: {iterations}"))?;
    if iterations == 0 {
        bail!("invalid iterations: {iterations}");
    }

    Ok((input_path, iterations))
}

fn benchmark_render_frame(
    search: &SearchInterface<'_>,
    query: &str,
    scratch: &mut RenderScratch,
    config: &Config,
) -> usize {
    let total_lines = search.lines.len().min(VISIBLE_LINES);
    let current_match = search.first_visible_match(total_lines);
    let mut checksum = 0usize;

    for line_idx in 0..total_lines {
        checksum += checksum_rendered_line(
            search.lines[line_idx],
            search.get_matches_at_line(line_idx),
            current_match,
            scratch,
        );
    }

    checksum_cstr(&mut checksum, "P");
    checksum_cstr(&mut checksum, &config.prompt_indicator);
    checksum_cstr(&mut checksum, " ");
    if query.is_empty() {
        checksum_cstr(&mut checksum, &config.prompt_placeholder_text);
    } else {
        checksum_bytes(&mut checksum, query.as_bytes());
    }

    checksum
}

fn checksum_rendered_line(
    line: &str,
    matches: &[SearchMatch<'_>],
    current_match: Option<&SearchMatch<'_>>,
    scratch: &mut RenderScratch,
) -> usize {
    let mut checksum = 0usize;
    if line.is_empty() {
        checksum_style_marker(&mut checksum, StyleKind::Base);
        return checksum;
    }

    if matches.is_empty() {
        checksum_style_marker(&mut checksum, StyleKind::Base);
        checksum_bytes(&mut checksum, line.as_bytes());
        return checksum;
    }

    scratch.prepare(line.len());

    for m in matches {
        if let Some(label) = m.label {
            let pos = m.col + m.match_end;
            if pos <= line.len() {
                scratch.labels.push(LabelPosition { pos, label });
            }
        }

        let style = if current_match.is_some_and(|current| same_match(current, m)) {
            StyleKind::Current
        } else {
            StyleKind::Highlight
        };
        let start = (m.col + m.match_start).min(line.len());
        let end = (m.col + m.match_end).min(line.len());
        for slot in scratch.style_map.iter_mut().take(end).skip(start) {
            if style > *slot {
                *slot = style;
            }
        }
    }

    scratch.labels.sort_by_key(|label| label.pos);
    scratch.labels.dedup_by_key(|label| label.pos);

    let mut label_idx = 0usize;
    let mut active = StyleKind::Base;
    checksum_style_marker(&mut checksum, active);

    for (idx, byte) in line.as_bytes().iter().copied().enumerate() {
        if label_idx < scratch.labels.len() && scratch.labels[label_idx].pos == idx {
            checksum_label(&mut checksum, scratch.labels[label_idx].label);
            label_idx += 1;
            continue;
        }

        let style = scratch.style_map[idx];
        if style != active {
            active = style;
            checksum_style_marker(&mut checksum, active);
        }

        checksum_bytes(&mut checksum, &[byte]);
    }

    if label_idx < scratch.labels.len() && scratch.labels[label_idx].pos == line.len() {
        checksum_label(&mut checksum, scratch.labels[label_idx].label);
    }

    checksum
}

impl RenderScratch {
    fn prepare(&mut self, line_len: usize) {
        self.labels.clear();
        self.style_map.resize(line_len, StyleKind::Base);
        self.style_map.fill(StyleKind::Base);
    }
}

fn same_match(left: &SearchMatch<'_>, right: &SearchMatch<'_>) -> bool {
    left.line == right.line
        && left.col == right.col
        && left.match_start == right.match_start
        && left.match_end == right.match_end
}

fn checksum_style_marker(checksum: &mut usize, style: StyleKind) {
    let marker = match style {
        StyleKind::Base => b'B',
        StyleKind::Highlight => b'H',
        StyleKind::Current => b'C',
    };
    checksum_bytes(checksum, &[marker]);
}

fn checksum_label(checksum: &mut usize, label: char) {
    let mut buf = [0u8; 4];
    let bytes = label.encode_utf8(&mut buf).as_bytes();
    checksum_bytes(checksum, b"L");
    checksum_bytes(checksum, bytes);
}

fn checksum_cstr(checksum: &mut usize, text: &str) {
    checksum_bytes(checksum, text.as_bytes());
}

fn checksum_bytes(checksum: &mut usize, bytes: &[u8]) {
    for byte in bytes {
        *checksum = checksum.wrapping_mul(131).wrapping_add(usize::from(*byte));
    }
}
