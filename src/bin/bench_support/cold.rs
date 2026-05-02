use anyhow::{Result, bail};
use flash_tmux::search::{SearchInterface, SearchMatch};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Mode {
    Init,
    Search1,
    Search2,
    Search3,
    Backspace2,
    Backspace3,
    Render1,
    Render2,
    Render3,
}

impl Mode {
    pub fn parse(value: &str) -> Result<Self> {
        match value {
            "init" => Ok(Self::Init),
            "search-1" => Ok(Self::Search1),
            "search-2" => Ok(Self::Search2),
            "search-3" => Ok(Self::Search3),
            "backspace-2" => Ok(Self::Backspace2),
            "backspace-3" => Ok(Self::Backspace3),
            "render-1" => Ok(Self::Render1),
            "render-2" => Ok(Self::Render2),
            "render-3" => Ok(Self::Render3),
            _ => bail!("unknown cold selection mode {value:?}"),
        }
    }

    pub fn depth(self) -> usize {
        match self {
            Self::Init => 0,
            Self::Search1 | Self::Render1 => 1,
            Self::Search2 | Self::Backspace2 | Self::Render2 => 2,
            Self::Search3 | Self::Backspace3 | Self::Render3 => 3,
        }
    }

    pub fn backspace_depth(self) -> Option<usize> {
        match self {
            Self::Backspace2 => Some(1),
            Self::Backspace3 => Some(2),
            _ => None,
        }
    }

    pub fn renders(self) -> bool {
        matches!(self, Self::Render1 | Self::Render2 | Self::Render3)
    }
}

pub fn search_step_checksum(search: &mut SearchInterface<'_>, query: &str) -> usize {
    let matches = search.search(query);
    let mut checksum = matches.len().wrapping_add(query.len());
    if let Some(first) = matches.first() {
        checksum = checksum.wrapping_add(match_checksum(first));
    }
    checksum
}

pub fn selection_checksum(search: &SearchInterface<'_>, visible_lines: usize) -> usize {
    let label = search
        .first_visible_match(visible_lines)
        .and_then(|first| first.label)
        .or_else(|| {
            (0..visible_lines)
                .flat_map(|line| search.get_matches_at_line(line))
                .find_map(|candidate| candidate.label)
        })
        .expect("every cold selection trace must end with a visible label");
    let selected = search
        .get_match_by_label(label)
        .expect("a visible label must resolve through the label index");

    match_checksum(selected)
        .wrapping_add(selected.text.len())
        .wrapping_add(ascii_label_checksum(label))
}

fn match_checksum(first: &SearchMatch<'_>) -> usize {
    first
        .line
        .wrapping_add(first.col)
        .wrapping_add(first.match_start)
        .wrapping_add(first.match_end)
        .wrapping_add(first.label.map_or(0, ascii_label_checksum))
}

fn ascii_label_checksum(label: char) -> usize {
    let mut buf = [0u8; 4];
    usize::from(label.encode_utf8(&mut buf).as_bytes()[0])
}
