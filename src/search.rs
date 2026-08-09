#[derive(Clone, Copy, Debug)]
pub struct SearchMatch<'a> {
    pub text: &'a str,
    pub line: usize,
    pub col: usize,
    pub label: Option<char>,
    pub token_index: u32,
    pub match_start: usize,
    pub match_end: usize,
}

#[derive(Clone, Copy, Debug)]
struct SearchToken<'a> {
    text: &'a str,
    selection: &'a str,
    selection_group: usize,
    line: usize,
    col: usize,
}

#[derive(Clone, Copy)]
struct LabelCandidate {
    label: u8,
    lower: u8,
}

#[derive(Clone, Copy)]
struct LabeledSelection<'a> {
    text: &'a str,
    label: u8,
}

#[derive(Clone, Copy)]
struct QueryByteMatcher {
    lower: u8,
    upper: u8,
}

const ASCII_LOWER_BYTES: [u8; 256] = build_ascii_lower_bytes();
const QUERY_STACK_CAPACITY: usize = 256;
const ESTIMATED_BYTES_PER_TOKEN: usize = 8;
const MEMCHR_CANDIDATE_THRESHOLD: usize = 40;

const fn build_ascii_lower_bytes() -> [u8; 256] {
    let mut bytes = [0; 256];
    let mut byte = 0u8;
    loop {
        bytes[byte as usize] = if byte >= b'A' && byte <= b'Z' {
            byte + (b'a' - b'A')
        } else {
            byte
        };
        if byte == u8::MAX {
            break;
        }
        byte += 1;
    }
    bytes
}

impl QueryByteMatcher {
    fn new(query_lower: u8) -> Self {
        let upper = if query_lower.is_ascii_lowercase() {
            query_lower - (b'a' - b'A')
        } else {
            query_lower
        };
        Self {
            lower: query_lower,
            upper,
        }
    }

    fn matches(self, byte: u8) -> bool {
        byte == self.lower || byte == self.upper
    }
}

#[derive(Debug)]
pub struct SearchInterface<'a> {
    pub lines: Vec<&'a str>,
    tokens: Vec<SearchToken<'a>>,
    matches: Vec<SearchMatch<'a>>,
    line_match_ranges: Vec<(usize, usize)>,
    label_match_indices: [Option<usize>; 256],
    selection_group_labels: Vec<Option<u8>>,
    label_chars: String,
    trimmable_chars: String,
    last_query: String,
}

impl<'a> SearchInterface<'a> {
    pub fn new(pane_content: &'a str, label_chars: String) -> Self {
        Self::new_with_trimmable_chars(
            pane_content,
            label_chars,
            crate::config::DEFAULT_TRIMMABLE_CHARS.to_string(),
        )
    }

    pub fn new_with_trimmable_chars(
        pane_content: &'a str,
        label_chars: String,
        trimmable_chars: String,
    ) -> Self {
        Self::new_with_trimmable_chars_and_line_capacity(
            pane_content,
            label_chars,
            trimmable_chars,
            0,
        )
    }

    pub fn new_with_trimmable_chars_and_line_capacity(
        pane_content: &'a str,
        label_chars: String,
        trimmable_chars: String,
        line_capacity: usize,
    ) -> Self {
        let mut lines = Vec::with_capacity(line_capacity);
        lines.extend(pane_content.split('\n'));
        let (tokens, selection_group_count) = build_tokens(&lines, &trimmable_chars);
        let line_match_ranges = vec![(0, 0); lines.len()];
        Self {
            lines,
            tokens,
            matches: Vec::new(),
            line_match_ranges,
            label_match_indices: [None; 256],
            selection_group_labels: vec![None; selection_group_count],
            label_chars,
            trimmable_chars,
            last_query: String::new(),
        }
    }

    pub fn search(&mut self, query: &str) -> &[SearchMatch<'a>] {
        if query.is_empty() {
            self.matches.clear();
            self.clear_line_match_ranges();
            self.clear_label_match_indices();
            self.last_query.clear();
            return &self.matches;
        }

        let previous_query_len = self.last_query.len();
        let mut previous_query_stack = [0u8; QUERY_STACK_CAPACITY];
        let previous_query_heap;
        let previous_query = if previous_query_len <= QUERY_STACK_CAPACITY {
            let previous_query = &mut previous_query_stack[..previous_query_len];
            previous_query.copy_from_slice(self.last_query.as_bytes());
            &*previous_query
        } else {
            previous_query_heap = self.last_query.as_bytes().to_vec();
            &previous_query_heap
        };

        let mut query_cmp = std::mem::take(&mut self.last_query);
        query_cmp.clear();
        query_cmp.push_str(query);
        query_cmp.make_ascii_lowercase();
        let query_bytes = query_cmp.as_bytes();
        let query_len = query_bytes.len();

        if !previous_query.is_empty() && query_bytes.starts_with(previous_query) {
            self.refine_matches(query_bytes, previous_query_len);
        } else {
            self.matches.clear();
            self.scan_matches(query_bytes, query_len);
        }

        assign_labels(
            &mut self.matches,
            &self.tokens,
            &mut self.selection_group_labels,
            query_bytes,
            &self.label_chars,
            &self.trimmable_chars,
        );
        self.rebuild_label_match_indices();
        self.rebuild_line_match_ranges();
        self.last_query = query_cmp;

        &self.matches
    }

    pub fn get_match_by_label(&self, label: char) -> Option<&SearchMatch<'a>> {
        if !label.is_ascii() {
            return None;
        }

        self.label_match_indices[usize::from(label as u8)].and_then(|idx| self.matches.get(idx))
    }

    pub fn first_visible_match(&self, max_lines: usize) -> Option<&SearchMatch<'a>> {
        self.matches.iter().find(|m| m.line < max_lines)
    }

    pub fn get_matches_at_line(&self, line_num: usize) -> &[SearchMatch<'a>] {
        let Some((start, end)) = self.line_match_ranges.get(line_num).copied() else {
            return &[];
        };
        &self.matches[start..end]
    }

    fn clear_line_match_ranges(&mut self) {
        self.line_match_ranges.fill((0, 0));
    }

    fn clear_label_match_indices(&mut self) {
        self.label_match_indices.fill(None);
    }

    fn rebuild_label_match_indices(&mut self) {
        self.clear_label_match_indices();
        for (idx, m) in self.matches.iter().enumerate() {
            if let Some(label) = m.label
                && label.is_ascii()
            {
                let slot = &mut self.label_match_indices[usize::from(label as u8)];
                if slot.is_none() {
                    *slot = Some(idx);
                }
            }
        }
    }

    fn rebuild_line_match_ranges(&mut self) {
        self.clear_line_match_ranges();

        let mut start = 0usize;
        while start < self.matches.len() {
            let line = self.matches[start].line;
            let mut end = start + 1;
            while end < self.matches.len() && self.matches[end].line == line {
                end += 1;
            }

            if let Some(range) = self.line_match_ranges.get_mut(line) {
                *range = (start, end);
            }
            start = end;
        }
    }

    fn scan_matches(&mut self, query_bytes: &[u8], query_len: usize) {
        let first_query_byte = QueryByteMatcher::new(query_bytes[0]);
        let matches = &mut self.matches;

        for (token_index, token) in self.tokens.iter().enumerate().rev() {
            let token_bytes = token.text.as_bytes();
            if query_len > token_bytes.len() {
                continue;
            }

            let candidate_count = token_bytes.len() - query_len + 1;
            visit_candidate_positions(
                token_bytes,
                candidate_count,
                first_query_byte,
                |match_pos| {
                    let remainder_matches = query_len == 1
                        || ascii_case_insensitive_eq_lower(
                            &token_bytes[match_pos + 1..match_pos + query_len],
                            &query_bytes[1..],
                        );
                    if remainder_matches
                        && is_utf8_boundary(token_bytes, match_pos)
                        && is_utf8_boundary(token_bytes, match_pos + query_len)
                    {
                        let token_index = u32::try_from(token_index)
                            .expect("pane content contains too many search tokens");
                        matches.push(SearchMatch {
                            text: token.text,
                            line: token.line,
                            col: token.col,
                            label: None,
                            token_index,
                            match_start: match_pos,
                            match_end: match_pos + query_len,
                        });
                    }
                },
            );
        }
    }

    fn refine_matches(&mut self, query_bytes: &[u8], previous_query_len: usize) {
        self.matches.retain(|candidate| {
            let token_bytes = candidate.text.as_bytes();
            let extended_end = candidate.match_start + query_bytes.len();
            if extended_end > token_bytes.len() || !is_utf8_boundary(token_bytes, extended_end) {
                return false;
            }

            ascii_case_insensitive_eq_lower(
                &token_bytes[candidate.match_start + previous_query_len..extended_end],
                &query_bytes[previous_query_len..],
            )
        });

        for candidate in &mut self.matches {
            candidate.match_end = candidate.match_start + query_bytes.len();
        }
    }
}

fn visit_candidate_positions(
    token_bytes: &[u8],
    candidate_count: usize,
    first_query_byte: QueryByteMatcher,
    mut visit: impl FnMut(usize),
) {
    if candidate_count >= MEMCHR_CANDIDATE_THRESHOLD {
        for match_pos in memchr::memrchr2_iter(
            first_query_byte.lower,
            first_query_byte.upper,
            &token_bytes[..candidate_count],
        ) {
            visit(match_pos);
        }
        return;
    }

    let mut match_pos = candidate_count;
    while match_pos > 0 {
        match_pos -= 1;
        if first_query_byte.matches(token_bytes[match_pos]) {
            visit(match_pos);
        }
    }
}

fn build_tokens<'a>(lines: &[&'a str], trimmable_chars: &str) -> (Vec<SearchToken<'a>>, usize) {
    let content_bytes = lines.iter().map(|line| line.len()).sum::<usize>();
    let token_capacity = content_bytes
        .div_ceil(ESTIMATED_BYTES_PER_TOKEN)
        .max(lines.len());
    let mut selection_groups =
        rustc_hash::FxHashMap::with_capacity_and_hasher(token_capacity, rustc_hash::FxBuildHasher);
    let mut tokens = Vec::with_capacity(token_capacity);
    let ascii_trimmable_chars = ascii_trimmable_char_table(trimmable_chars);

    for (line_idx, line) in lines.iter().copied().enumerate() {
        let line_bytes = line.as_bytes();
        let mut idx = 0usize;

        while idx < line_bytes.len() {
            while idx < line_bytes.len() && is_ascii_whitespace(line_bytes[idx]) {
                idx += 1;
            }
            if idx >= line_bytes.len() {
                break;
            }

            let token_start = idx;
            while idx < line_bytes.len() && !is_ascii_whitespace(line_bytes[idx]) {
                idx += 1;
            }

            let text = &line[token_start..idx];
            let selection = if let Some(table) = &ascii_trimmable_chars {
                trim_token_ascii(text, table)
            } else {
                trim_token(text, trimmable_chars)
            };
            let next_group = selection_groups.len();
            let selection_group = *selection_groups.entry(selection).or_insert(next_group);
            tokens.push(SearchToken {
                text,
                selection,
                selection_group,
                line: line_idx,
                col: token_start,
            });
        }
    }

    (tokens, selection_groups.len())
}

pub fn delete_prev_word(input: &str) -> String {
    let mut delimiters = [false; 256];
    for byte in b"-_.,;:!?/\\()[]{}" {
        delimiters[usize::from(*byte)] = true;
    }

    let mut chars: Vec<char> = input.chars().collect();
    let mut end = chars.len();

    while end > 0 && chars[end - 1].is_whitespace() {
        end -= 1;
    }

    while end > 0
        && !chars[end - 1].is_whitespace()
        && !is_ascii_delimiter(chars[end - 1], &delimiters)
    {
        end -= 1;
    }

    while end > 0 && is_ascii_delimiter(chars[end - 1], &delimiters) {
        end -= 1;
    }

    chars.truncate(end);
    chars.into_iter().collect()
}

pub fn trim_wrapping_token<'a>(
    token: &'a str,
    match_start: usize,
    match_end: usize,
    trimmable_chars: &str,
) -> &'a str {
    let mut start = 0usize;
    for (idx, ch) in token.char_indices() {
        if idx >= match_start {
            break;
        }
        if is_leading_trimmable(ch, trimmable_chars) {
            start = idx + ch.len_utf8();
        } else {
            break;
        }
    }

    let mut end = token.len();
    while end > match_end {
        let Some((idx, ch)) = token[..end].char_indices().last() else {
            break;
        };
        if idx < match_end {
            break;
        }
        if trimmable_chars.contains(ch) {
            end = idx;
        } else {
            break;
        }
    }

    if start >= end {
        token
    } else {
        &token[start..end]
    }
}

fn trim_token<'a>(token: &'a str, trimmable_chars: &str) -> &'a str {
    let mut start = 0usize;
    for (idx, ch) in token.char_indices() {
        if is_leading_trimmable(ch, trimmable_chars) {
            start = idx + ch.len_utf8();
        } else {
            break;
        }
    }

    let mut end = token.len();
    while end > start {
        let Some((idx, ch)) = token[..end].char_indices().last() else {
            break;
        };
        if trimmable_chars.contains(ch) {
            end = idx;
        } else {
            break;
        }
    }

    if start >= end {
        token
    } else {
        &token[start..end]
    }
}

fn trim_token_ascii<'a>(token: &'a str, trimmable_chars: &[bool; 256]) -> &'a str {
    let bytes = token.as_bytes();
    let mut start = 0usize;
    while start < bytes.len() {
        let byte = bytes[start];
        if !byte.is_ascii() || byte == b'.' || !trimmable_chars[usize::from(byte)] {
            break;
        }
        start += 1;
    }

    let mut end = bytes.len();
    while end > start {
        let byte = bytes[end - 1];
        if !byte.is_ascii() || !trimmable_chars[usize::from(byte)] {
            break;
        }
        end -= 1;
    }

    if start >= end {
        token
    } else {
        &token[start..end]
    }
}

fn is_ascii_delimiter(ch: char, delimiters: &[bool; 256]) -> bool {
    ch.is_ascii() && delimiters[usize::from(ch as u8)]
}

fn is_leading_trimmable(ch: char, trimmable_chars: &str) -> bool {
    ch != '.' && trimmable_chars.contains(ch)
}

fn is_ascii_whitespace(byte: u8) -> bool {
    matches!(byte, b' ' | b'\t' | b'\n' | b'\r' | 0x0c | 0x0b)
}

fn is_utf8_boundary(text: &[u8], idx: usize) -> bool {
    idx == 0 || idx >= text.len() || (text[idx] & 0b1100_0000) != 0b1000_0000
}

fn ascii_lower_byte(byte: u8) -> u8 {
    ASCII_LOWER_BYTES[usize::from(byte)]
}

fn ascii_case_insensitive_eq_lower(left: &[u8], right_lower: &[u8]) -> bool {
    if left.len() != right_lower.len() {
        return false;
    }

    let mut idx = 0usize;
    while idx < left.len() {
        if !QueryByteMatcher::new(right_lower[idx]).matches(left[idx]) {
            return false;
        }
        idx += 1;
    }
    true
}

fn find_labeled_selection(selections: &[LabeledSelection<'_>], text: &str) -> Option<u8> {
    selections
        .iter()
        .find(|selection| selection.text == text)
        .map(|selection| selection.label)
}

fn trim_wrapping_token_ascii<'a>(
    token: &'a str,
    match_start: usize,
    match_end: usize,
    trimmable_chars: &[bool; 256],
) -> &'a str {
    let bytes = token.as_bytes();
    let mut start = 0usize;
    while start < match_start {
        let byte = bytes[start];
        if !byte.is_ascii() || byte == b'.' || !trimmable_chars[usize::from(byte)] {
            break;
        }
        start += 1;
    }

    let mut end = bytes.len();
    while end > match_end {
        let byte = bytes[end - 1];
        if !byte.is_ascii() || !trimmable_chars[usize::from(byte)] {
            break;
        }
        end -= 1;
    }

    if start >= end {
        token
    } else {
        &token[start..end]
    }
}

fn ascii_trimmable_char_table(trimmable_chars: &str) -> Option<[bool; 256]> {
    let mut table = [false; 256];
    for ch in trimmable_chars.chars() {
        if !ch.is_ascii() {
            return None;
        }
        table[usize::from(ch as u8)] = true;
    }
    Some(table)
}

fn label_conflicts_outside_selection(token: &str, selection: &str, label: u8) -> bool {
    if token.len() == selection.len() {
        return false;
    }

    let selection_start = selection.as_ptr() as usize - token.as_ptr() as usize;
    let selection_end = selection_start + selection.len();
    token.as_bytes()[..selection_start]
        .iter()
        .chain(&token.as_bytes()[selection_end..])
        .any(|byte| ascii_lower_byte(*byte) == label)
}

fn assign_labels(
    matches: &mut [SearchMatch<'_>],
    tokens: &[SearchToken<'_>],
    selection_group_labels: &mut [Option<u8>],
    query: &[u8],
    label_chars: &str,
    trimmable_chars: &str,
) {
    let mut query_chars = [false; 256];
    let mut continuation_chars = [false; 256];
    let mut used_labels = [false; 256];
    let mut token_chars = [false; 256];
    let mut candidates = [LabelCandidate { label: 0, lower: 0 }; 256];
    let mut candidates_len = 0usize;
    let mut used_candidates = 0usize;
    let mut token_chars_id: Option<(*const u8, usize)> = None;
    let mut labeled_selections = [LabeledSelection { text: "", label: 0 }; 256];
    let mut labeled_selections_len = 0usize;
    let ascii_trimmable_chars = ascii_trimmable_char_table(trimmable_chars);
    selection_group_labels.fill(None);

    for byte in query {
        query_chars[usize::from(*byte)] = true;
    }

    for m in matches.iter() {
        if m.match_end < m.text.len() {
            let next = ascii_lower_byte(m.text.as_bytes()[m.match_end]);
            continuation_chars[usize::from(next)] = true;
        }
    }

    for label in label_chars.bytes() {
        let lower = ascii_lower_byte(label);
        if query_chars[usize::from(lower)] || continuation_chars[usize::from(lower)] {
            continue;
        }
        if candidates_len < candidates.len() {
            candidates[candidates_len] = LabelCandidate { label, lower };
            candidates_len += 1;
        }
    }

    for m in matches.iter_mut() {
        m.label = None;
        let token_id = (m.text.as_ptr(), m.text.len());
        let token_index =
            usize::try_from(m.token_index).expect("token index must fit in pointer width");
        let token = &tokens[token_index];
        let selection_start = token.selection.as_ptr() as usize - token.text.as_ptr() as usize;
        let selection_end = selection_start + token.selection.len();
        let uses_token_selection = m.match_start >= selection_start && m.match_end <= selection_end;
        let selected = if uses_token_selection {
            token.selection
        } else if let Some(table) = &ascii_trimmable_chars {
            trim_wrapping_token_ascii(m.text, m.match_start, m.match_end, table)
        } else {
            trim_wrapping_token(m.text, m.match_start, m.match_end, trimmable_chars)
        };

        let existing_label = if uses_token_selection {
            selection_group_labels[token.selection_group]
        } else {
            find_labeled_selection(&labeled_selections[..labeled_selections_len], selected)
        };
        if let Some(label) = existing_label {
            if !label_conflicts_outside_selection(m.text, selected, ascii_lower_byte(label)) {
                m.label = Some(char::from(label));
            }
            continue;
        }
        if used_candidates == candidates_len {
            continue;
        }

        if token_chars_id != Some(token_id) {
            token_chars.fill(false);
            for byte in m.text.bytes() {
                token_chars[usize::from(ascii_lower_byte(byte))] = true;
            }
            token_chars_id = Some(token_id);
        }

        for candidate in &candidates[..candidates_len] {
            if used_labels[usize::from(candidate.label)]
                || token_chars[usize::from(candidate.lower)]
            {
                continue;
            }

            m.label = Some(char::from(candidate.label));
            used_labels[usize::from(candidate.label)] = true;
            used_candidates += 1;
            if uses_token_selection {
                selection_group_labels[token.selection_group] = Some(candidate.label);
            } else {
                labeled_selections[labeled_selections_len] = LabeledSelection {
                    text: selected,
                    label: candidate.label,
                };
                labeled_selections_len += 1;
            }
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;

    fn default_labels() -> String {
        Config::defaults().label_characters
    }

    fn default_trimmable() -> String {
        Config::defaults().trimmable_chars
    }

    fn match_signatures<'a>(
        matches: &[SearchMatch<'a>],
    ) -> Vec<(&'a str, usize, usize, u32, usize, usize, Option<char>)> {
        matches
            .iter()
            .map(|m| {
                (
                    m.text,
                    m.line,
                    m.col,
                    m.token_index,
                    m.match_start,
                    m.match_end,
                    m.label,
                )
            })
            .collect()
    }

    #[test]
    fn query_byte_matcher_matches_ascii_lower_equivalence() {
        for query_byte in 0u8..=u8::MAX {
            let query_lower = ascii_lower_byte(query_byte);
            let matcher = QueryByteMatcher::new(query_lower);
            for token_byte in 0u8..=u8::MAX {
                assert_eq!(
                    matcher.matches(token_byte),
                    ascii_lower_byte(token_byte) == query_lower,
                    "query_byte={query_byte} token_byte={token_byte}"
                );
            }
        }
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn search_match_size_remains_unchanged_with_token_index() {
        assert_eq!(std::mem::size_of::<SearchMatch<'_>>(), 56);
    }

    #[test]
    fn search_case_insensitive() {
        let mut search = SearchInterface::new("Foo bar", default_labels());
        let matches = search.search("fo");
        assert_eq!(matches.len(), 1);
        let m = &matches[0];
        assert_eq!(m.text, "Foo");
        assert_eq!(m.line, 0);
        assert_eq!(m.col, 0);
        assert_eq!(m.match_start, 0);
        assert_eq!(m.match_end, 2);
        assert_eq!(m.label, Some('j'));
    }

    #[test]
    fn search_splits_on_ascii_whitespace() {
        let mut search = SearchInterface::new("alpha\tbeta gamma", default_labels());
        let matches = search.search("be");

        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].text, "beta");
        assert_eq!(matches[0].col, 6);
    }

    #[test]
    fn search_does_not_split_on_non_ascii_whitespace() {
        let mut search = SearchInterface::new("alpha\u{00a0}beta", default_labels());
        let matches = search.search("be");

        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].text, "alpha\u{00a0}beta");
        assert_eq!(matches[0].col, 0);
        assert_eq!(matches[0].match_start, "alpha\u{00a0}".len());
    }

    #[test]
    fn search_case_folding_is_ascii_only() {
        let mut search = SearchInterface::new("Ärger ärger", default_labels());

        assert!(search.search("är").len() == 1);
        assert!(search.search("ÄR").len() == 1);
        assert!(search.search("är").iter().all(|m| m.text == "ärger"));
        assert!(search.search("ÄR").iter().all(|m| m.text == "Ärger"));
    }

    #[test]
    fn search_folds_ascii_suffix_but_not_non_ascii_prefix() {
        let mut search = SearchInterface::new("éA éa ÉA Éa", default_labels());
        let matches = search.search("éa");
        let texts: Vec<_> = matches.iter().map(|m| m.text).collect();

        assert_eq!(texts, vec!["éa", "éA"]);
    }

    #[test]
    fn search_handles_long_ascii_queries() {
        for query_len in [256, 257] {
            let query = "A".repeat(query_len);
            let pane = "a".repeat(query_len);
            let mut search = SearchInterface::new(&pane, default_labels());

            let matches = search.search(&query);

            assert_eq!(matches.len(), 1);
            assert_eq!(matches[0].match_start, 0);
            assert_eq!(matches[0].match_end, query_len);
            assert_eq!(search.last_query, pane);
        }
    }

    #[test]
    fn search_long_query_preserves_utf8_and_ascii_folding() {
        let prefix = "é".repeat(128);
        let query = format!("{prefix}A");
        let pane = format!("{prefix}a");
        assert_eq!(query.len(), 257);
        let mut search = SearchInterface::new(&pane, default_labels());

        let matches = search.search(&query);

        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].match_start, 0);
        assert_eq!(matches[0].match_end, query.len());
        assert_eq!(search.last_query, pane);
    }

    #[test]
    fn search_reuses_last_query_buffer() {
        let mut search = SearchInterface::new("alpha alphabet", default_labels());
        search.search("Alphabet");
        let query_ptr = search.last_query.as_ptr();
        let query_capacity = search.last_query.capacity();

        search.search("Al");

        assert_eq!(search.last_query.as_ptr(), query_ptr);
        assert_eq!(search.last_query.capacity(), query_capacity);
    }

    #[test]
    fn search_ordering_is_reverse() {
        let mut search = SearchInterface::new("abc abc", default_labels());
        let matches = search.search("a");
        assert_eq!(matches.len(), 2);
        assert_eq!(matches[0].col, 4);
        assert_eq!(matches[1].col, 0);
    }

    #[test]
    fn search_ordering_is_reverse_across_lines_tokens_and_offsets() {
        let mut search = SearchInterface::new("banana bandana\nanagram banana", default_labels());
        let matches = search.search("ana");
        let positions: Vec<_> = matches
            .iter()
            .map(|m| (m.line, m.col, m.match_start))
            .collect();

        assert_eq!(
            positions,
            vec![
                (1, 8, 3),
                (1, 8, 1),
                (1, 0, 0),
                (0, 7, 4),
                (0, 0, 3),
                (0, 0, 1)
            ]
        );
    }

    #[test]
    fn search_ordering_is_reverse_for_overlapping_offsets() {
        let mut search = SearchInterface::new("aaaa", default_labels());
        let matches = search.search("aa");
        let starts: Vec<_> = matches.iter().map(|m| m.match_start).collect();

        assert_eq!(starts, vec![2, 1, 0]);
    }

    #[test]
    fn search_at_memchr_threshold_preserves_reverse_overlapping_order() {
        let pane = "a".repeat(MEMCHR_CANDIDATE_THRESHOLD + 1);
        let mut search = SearchInterface::new(&pane, default_labels());
        let matches = search.search("AA");
        let starts: Vec<_> = matches.iter().map(|m| m.match_start).collect();
        let expected: Vec<_> = (0..MEMCHR_CANDIDATE_THRESHOLD).rev().collect();

        assert_eq!(starts, expected);
    }

    #[test]
    fn search_matches_reference_originating_tokens() {
        let mut search = SearchInterface::new("alpha beta\nalphabet gamma", default_labels());
        search.search("a");

        for search_match in &search.matches {
            let token_index =
                usize::try_from(search_match.token_index).expect("token index should fit");
            let token = &search.tokens[token_index];
            assert_eq!(token.text.as_ptr(), search_match.text.as_ptr());
            assert_eq!(token.text.len(), search_match.text.len());
            assert_eq!(token.line, search_match.line);
            assert_eq!(token.col, search_match.col);
        }
    }

    #[test]
    fn rescanned_matches_keep_originating_token_indices() {
        let mut search = SearchInterface::new("alpha beta\nalphabet gamma", default_labels());
        search.search("a");
        search.search("al");
        search.search("a");

        for search_match in &search.matches {
            let token_index =
                usize::try_from(search_match.token_index).expect("token index should fit");
            let token = &search.tokens[token_index];
            assert_eq!(token.text.as_ptr(), search_match.text.as_ptr());
            assert_eq!(token.text.len(), search_match.text.len());
        }
    }

    #[test]
    fn search_does_not_emit_duplicate_matches() {
        let mut search = SearchInterface::new("abc abc abc", default_labels());
        let matches = search.search("a");

        for (idx, left) in matches.iter().enumerate() {
            for right in &matches[idx + 1..] {
                assert!(
                    left.line != right.line
                        || left.col != right.col
                        || left.match_start != right.match_start
                        || !std::ptr::eq(left.text.as_ptr(), right.text.as_ptr())
                        || left.text.len() != right.text.len()
                );
            }
        }
    }

    #[test]
    fn search_matches_keep_utf8_boundaries() {
        let mut search = SearchInterface::new("aé café ÉA éa", default_labels());
        let matches = search.search("é");

        assert_eq!(matches.len(), 3);
        for m in matches {
            assert!(m.text.is_char_boundary(m.match_start));
            assert!(m.text.is_char_boundary(m.match_end));
        }
    }

    #[test]
    fn get_matches_at_line_returns_borrowed_slice() {
        let mut search = SearchInterface::new("alpha beta\ngamma alpha", default_labels());
        search.search("al");

        let line_0 = search.get_matches_at_line(0);
        let line_1 = search.get_matches_at_line(1);
        let line_2 = search.get_matches_at_line(2);

        assert_eq!(line_0.len(), 1);
        assert!(line_0.iter().all(|m| m.line == 0));
        assert_eq!(line_1.len(), 1);
        assert!(line_1.iter().all(|m| m.line == 1));
        assert!(line_2.is_empty());
    }

    #[test]
    fn search_refines_when_query_extends_previous_query() {
        let pane = "alpha alphabet alphanumeric\nbeta alpha";
        let mut refined = SearchInterface::new(pane, default_labels());
        let mut fresh = SearchInterface::new(pane, default_labels());

        refined.search("al");
        let refined_matches = refined.search("alp");
        let fresh_matches = fresh.search("alp");

        assert_eq!(refined_matches.len(), fresh_matches.len());
        for (left, right) in refined_matches.iter().zip(fresh_matches.iter()) {
            assert_eq!(left.text, right.text);
            assert_eq!(left.line, right.line);
            assert_eq!(left.col, right.col);
            assert_eq!(left.token_index, right.token_index);
            assert_eq!(left.match_start, right.match_start);
            assert_eq!(left.match_end, right.match_end);
            assert_eq!(left.label, right.label);
        }
    }

    #[test]
    fn search_refines_utf8_query_like_fresh_scan() {
        let pane = "éA alpha\nbeta éa ÉA";
        let mut refined = SearchInterface::new(pane, default_labels());
        let mut fresh = SearchInterface::new(pane, default_labels());

        refined.search("é");
        let refined_matches: Vec<_> = refined
            .search("éa")
            .iter()
            .map(|m| {
                (
                    m.text,
                    m.line,
                    m.col,
                    m.token_index,
                    m.match_start,
                    m.match_end,
                    m.label,
                )
            })
            .collect();
        let fresh_matches: Vec<_> = fresh
            .search("éa")
            .iter()
            .map(|m| {
                (
                    m.text,
                    m.line,
                    m.col,
                    m.token_index,
                    m.match_start,
                    m.match_end,
                    m.label,
                )
            })
            .collect();

        assert_eq!(refined_matches, fresh_matches);
    }

    #[test]
    fn search_rescans_when_query_shrinks_to_previous_prefix() {
        let pane = "alpha alphabet alphanumeric\nbeta alphabet";
        let mut incremental = SearchInterface::new(pane, default_labels());
        let mut fresh = SearchInterface::new(pane, default_labels());

        incremental.search("a");
        incremental.search("al");
        incremental.search("alp");
        assert_eq!(
            match_signatures(incremental.search("al")),
            match_signatures(fresh.search("al"))
        );
    }

    #[test]
    fn search_rescans_for_long_query() {
        let shorter_query = "A".repeat(256);
        let longer_query = format!("{shorter_query}B");
        let pane = longer_query.to_ascii_lowercase();
        let mut incremental = SearchInterface::new(&pane, default_labels());
        let mut fresh = SearchInterface::new(&pane, default_labels());

        incremental.search(&shorter_query);
        incremental.search(&longer_query);
        assert_eq!(
            match_signatures(incremental.search(&shorter_query)),
            match_signatures(fresh.search(&shorter_query))
        );
    }

    #[test]
    fn search_backspace_recovers_matches_from_empty_set() {
        let pane = "zzz zzzzz alpha";
        let mut incremental = SearchInterface::new(pane, default_labels());
        let mut fresh = SearchInterface::new(pane, default_labels());

        assert!(incremental.search("zzzzzz").is_empty());
        assert_eq!(
            match_signatures(incremental.search("zzz")),
            match_signatures(fresh.search("zzz"))
        );
    }

    #[test]
    fn search_backspace_with_utf8_matches_fresh_scan() {
        let pane = "é éa café";
        let mut incremental = SearchInterface::new(pane, default_labels());
        let mut fresh = SearchInterface::new(pane, default_labels());

        incremental.search("éa");
        let rescanned = incremental.search("é");
        assert!(
            rescanned
                .iter()
                .all(|m| m.text.is_char_boundary(m.match_end))
        );
        assert_eq!(
            match_signatures(rescanned),
            match_signatures(fresh.search("é"))
        );
    }

    #[test]
    fn search_interior_edit_discovers_shifted_match() {
        let pane = "slash flash";
        let mut edited = SearchInterface::new(pane, default_labels());
        let mut fresh = SearchInterface::new(pane, default_labels());

        edited.search("lash");
        assert_eq!(
            match_signatures(edited.search("slash")),
            match_signatures(fresh.search("slash"))
        );
    }

    #[test]
    fn search_results_do_not_depend_on_backspace_history() {
        let pane = "alpha alphabet alphanumeric";
        let mut edited = SearchInterface::new(pane, default_labels());
        let mut fresh = SearchInterface::new(pane, default_labels());

        edited.search("a");
        edited.search("al");
        edited.search("a");
        assert_eq!(
            match_signatures(edited.search("al")),
            match_signatures(fresh.search("al"))
        );
    }

    #[test]
    fn backspace_rebuilds_label_lookup_like_fresh_scan() {
        let pane = "alpha alphabet alphanumeric";
        let mut edited = SearchInterface::new(pane, default_labels());
        let mut fresh = SearchInterface::new(pane, default_labels());

        edited.search("alp");
        let edited_labels: Vec<_> = edited.search("al").iter().filter_map(|m| m.label).collect();
        fresh.search("al");

        for label in edited_labels {
            assert_eq!(
                edited.get_match_by_label(label).map(|m| m.text),
                fresh.get_match_by_label(label).map(|m| m.text)
            );
        }
    }

    #[test]
    fn search_refinement_from_empty_match_set_stays_empty() {
        let mut search = SearchInterface::new("alpha beta", default_labels());
        assert!(search.search("zzz").is_empty());
        assert!(search.search("zzzz").is_empty());
    }

    #[test]
    fn search_falls_back_to_full_rescan_for_non_prefix_query() {
        let pane = "alpha beta gamma";
        let mut refined = SearchInterface::new(pane, default_labels());
        let mut fresh = SearchInterface::new(pane, default_labels());

        refined.search("al");
        let refined_matches = refined.search("be");
        let fresh_matches = fresh.search("be");

        assert_eq!(refined_matches.len(), fresh_matches.len());
        for (left, right) in refined_matches.iter().zip(fresh_matches.iter()) {
            assert_eq!(left.text, right.text);
            assert_eq!(left.line, right.line);
            assert_eq!(left.col, right.col);
            assert_eq!(left.match_start, right.match_start);
            assert_eq!(left.match_end, right.match_end);
            assert_eq!(left.label, right.label);
        }
    }

    #[test]
    fn labels_avoid_query_and_match_chars() {
        let mut search = SearchInterface::new("abc", default_labels());
        let matches = search.search("a");
        assert_eq!(matches.len(), 1);
        for m in matches {
            let label = m
                .label
                .expect("expected label to be assigned for test match");
            assert!(!"abc".contains(label));
        }
    }

    #[test]
    fn labels_avoid_continuation_chars() {
        let mut search = SearchInterface::new("fooj foo", default_labels());
        let matches = search.search("foo");

        assert_eq!(matches.len(), 2);
        assert!(matches.iter().all(|m| m.label != Some('j')));
    }

    #[test]
    fn uppercase_and_lowercase_configured_labels_are_distinct() {
        let mut search = SearchInterface::new("x X", "aA".to_string());
        let labels: Vec<_> = search.search("x").iter().map(|m| m.label).collect();

        assert_eq!(labels, vec![Some('a'), Some('A')]);
    }

    #[test]
    fn duplicate_configured_labels_are_used_once() {
        let mut search = SearchInterface::new("x2 X x1", "aab".to_string());
        let labels: Vec<_> = search.search("x").iter().map(|m| m.label).collect();

        assert_eq!(labels, vec![Some('a'), Some('b'), None]);
    }

    #[test]
    fn identical_trimmed_selections_reuse_label() {
        let mut search = SearchInterface::new("foo (foo) foo,", default_labels());
        let labels: Vec<_> = search.search("foo").iter().map(|m| m.label).collect();

        assert_eq!(labels, vec![Some('j'), Some('j'), Some('j')]);
    }

    #[test]
    fn selection_grouping_is_case_sensitive() {
        let mut search = SearchInterface::new("foo FOO", default_labels());
        let labels: Vec<_> = search.search("foo").iter().map(|m| m.label).collect();

        assert_eq!(labels, vec![Some('j'), Some('k')]);
    }

    #[test]
    fn different_trimmed_outputs_do_not_share_label() {
        let mut search = SearchInterface::new("((x))", default_labels());
        let matches = search.search("(");

        assert_eq!(matches.len(), 2);
        assert_eq!(
            trim_wrapping_token(
                matches[0].text,
                matches[0].match_start,
                matches[0].match_end,
                &default_trimmable(),
            ),
            "(x"
        );
        assert_eq!(
            trim_wrapping_token(
                matches[1].text,
                matches[1].match_start,
                matches[1].match_end,
                &default_trimmable(),
            ),
            "((x"
        );
        assert_eq!(matches[0].label, Some('j'));
        assert_eq!(matches[1].label, Some('k'));
    }

    #[test]
    fn duplicate_reuses_label_after_candidate_exhaustion() {
        let mut search = SearchInterface::new("foo foo", "j".to_string());
        let labels: Vec<_> = search.search("foo").iter().map(|m| m.label).collect();

        assert_eq!(labels, vec![Some('j'), Some('j')]);
    }

    #[test]
    fn shared_label_conflict_leaves_occurrence_unlabeled() {
        let mut search =
            SearchInterface::new_with_trimmable_chars("jfoo foo", "j".to_string(), "j".to_string());
        let labels: Vec<_> = search.search("foo").iter().map(|m| m.label).collect();

        assert_eq!(labels, vec![Some('j'), None]);
    }

    #[test]
    fn label_lookup_returns_first_shared_match() {
        let mut search = SearchInterface::new("foo foo", default_labels());
        search.search("foo");

        let found = search
            .get_match_by_label('j')
            .expect("expected shared label to resolve");

        assert_eq!(found.col, 4);
        assert_eq!(
            trim_wrapping_token(
                found.text,
                found.match_start,
                found.match_end,
                &default_trimmable(),
            ),
            "foo"
        );
    }

    #[test]
    fn token_local_label_rejection_is_not_global() {
        let mut search = SearchInterface::new("x jx", "jk".to_string());
        let labels: Vec<_> = search.search("x").iter().map(|m| m.label).collect();

        assert_eq!(labels, vec![Some('k'), Some('j')]);
    }

    #[test]
    fn label_lookup_returns_indexed_match() {
        let mut search = SearchInterface::new("alpha beta", default_labels());
        search.search("a");

        let found = search
            .get_match_by_label('j')
            .expect("expected first default label to resolve");

        assert_eq!(found.text, "beta");
        assert_eq!(found.label, Some('j'));
        assert!(search.get_match_by_label('J').is_none());
        assert!(search.get_match_by_label('é').is_none());
    }

    #[test]
    fn label_lookup_clears_when_query_clears() {
        let mut search = SearchInterface::new("alpha beta", default_labels());
        search.search("a");
        assert!(search.get_match_by_label('j').is_some());

        search.search("");

        assert!(search.get_match_by_label('j').is_none());
    }

    #[test]
    fn delete_prev_word_basic() {
        assert_eq!(delete_prev_word("foo bar"), "foo ");
    }

    #[test]
    fn delete_prev_word_trailing_spaces() {
        assert_eq!(delete_prev_word("foo bar   "), "foo ");
    }

    #[test]
    fn delete_prev_word_delimiters() {
        assert_eq!(delete_prev_word("foo-bar"), "foo");
        assert_eq!(delete_prev_word("foo/bar"), "foo");
    }

    #[test]
    fn find_tokens_basic() {
        let mut search = SearchInterface::new("alpha beta\ngamma", default_labels());
        let matches = search.search("a");
        assert!(!matches.is_empty());
        assert_eq!(search.lines, vec!["alpha beta", "gamma"]);
        assert_eq!(search.tokens.len(), 3);
    }

    #[test]
    fn ascii_token_trimming_matches_general_trimming() {
        let trimmable_chars = default_trimmable();
        let table = ascii_trimmable_char_table(&trimmable_chars).unwrap();

        for token in [
            "foo",
            "(foo)",
            "(`foo`)",
            ".gitignore",
            "foo...",
            "()",
            "(é)",
        ] {
            assert_eq!(
                trim_token_ascii(token, &table),
                trim_token(token, &trimmable_chars)
            );
        }
    }

    #[test]
    fn token_building_supports_non_ascii_trimmable_characters() {
        let lines = ["§foo§"];
        let (tokens, _) = build_tokens(&lines, "§");

        assert_eq!(tokens[0].selection, "foo");
    }

    #[test]
    fn trim_wrapping_token_basic() {
        assert_eq!(
            trim_wrapping_token("(foo)", 1, 4, &default_trimmable()),
            "foo"
        );
    }

    #[test]
    fn trim_wrapping_token_nested() {
        assert_eq!(
            trim_wrapping_token("(`foo`)", 2, 5, &default_trimmable()),
            "foo"
        );
    }

    #[test]
    fn trim_wrapping_token_trailing_only() {
        assert_eq!(
            trim_wrapping_token("foo...", 0, 3, &default_trimmable()),
            "foo"
        );
    }

    #[test]
    fn trim_wrapping_token_punctuation() {
        assert_eq!(
            trim_wrapping_token(",:foo.;", 2, 5, &default_trimmable()),
            "foo"
        );
    }

    #[test]
    fn trim_wrapping_token_preserves_leading_dot() {
        assert_eq!(
            trim_wrapping_token(".gitignore", 1, 4, &default_trimmable()),
            ".gitignore"
        );
    }

    #[test]
    fn trim_wrapping_token_preserves_leading_dots_but_trims_trailing_dot() {
        assert_eq!(
            trim_wrapping_token("../some_dir/.", 3, 11, &default_trimmable()),
            "../some_dir/"
        );
    }
}
