#[derive(Clone, Copy, Debug)]
pub struct SearchMatch<'a> {
    pub text: &'a str,
    pub line: usize,
    pub col: usize,
    pub label: Option<char>,
    pub match_start: usize,
    pub match_end: usize,
}

#[derive(Clone, Copy, Debug)]
struct SearchToken<'a> {
    text: &'a str,
    line: usize,
    col: usize,
}

#[derive(Debug)]
struct SearchSnapshot<'a> {
    query: String,
    matches: Vec<SearchMatch<'a>>,
}

#[derive(Clone, Copy)]
struct LabelCandidate {
    label: u8,
    lower: u8,
}

#[derive(Debug)]
pub struct SearchInterface<'a> {
    pub lines: Vec<&'a str>,
    tokens: Vec<SearchToken<'a>>,
    matches: Vec<SearchMatch<'a>>,
    snapshots: Vec<SearchSnapshot<'a>>,
    line_match_ranges: Vec<(usize, usize)>,
    label_match_indices: [Option<usize>; 256],
    label_chars: String,
    last_query: String,
}

impl<'a> SearchInterface<'a> {
    pub fn new(pane_content: &'a str, label_chars: String) -> Self {
        let lines: Vec<&str> = pane_content.split('\n').collect();
        let tokens = build_tokens(&lines);
        let line_match_ranges = vec![(0, 0); lines.len()];
        Self {
            lines,
            tokens,
            matches: Vec::new(),
            snapshots: Vec::new(),
            line_match_ranges,
            label_match_indices: [None; 256],
            label_chars,
            last_query: String::new(),
        }
    }

    pub fn search(&mut self, query: &str) -> &[SearchMatch<'a>] {
        if query.is_empty() {
            self.matches.clear();
            self.clear_line_match_ranges();
            self.clear_label_match_indices();
            self.last_query.clear();
            self.snapshots.clear();
            return &self.matches;
        }

        let query_cmp = query.to_ascii_lowercase();
        let query_bytes = query_cmp.as_bytes();
        let query_len = query_bytes.len();

        if !self.last_query.is_empty() && query_cmp.starts_with(&self.last_query) {
            if query_len > self.last_query.len() {
                self.store_snapshot();
            }
            self.refine_matches(query_bytes, self.last_query.len());
        } else if self.last_query.starts_with(&query_cmp) && self.restore_snapshot(&query_cmp) {
            // Snapshot restored; labels and line ranges are rebuilt below.
        } else {
            self.snapshots.clear();
            self.matches.clear();
            self.scan_matches(query_bytes, query_len);
        }

        assign_labels(&mut self.matches, query_bytes, &self.label_chars);
        self.rebuild_label_match_indices();
        self.rebuild_line_match_ranges();
        self.last_query = query_cmp;

        &self.matches
    }

    fn store_snapshot(&mut self) {
        if self.last_query.is_empty() {
            return;
        }

        if let Some(snapshot) = self
            .snapshots
            .iter_mut()
            .find(|snapshot| snapshot.query == self.last_query)
        {
            snapshot.matches.clone_from(&self.matches);
            return;
        }

        self.snapshots.push(SearchSnapshot {
            query: self.last_query.clone(),
            matches: self.matches.clone(),
        });
    }

    fn restore_snapshot(&mut self, query: &str) -> bool {
        let Some(snapshot) = self
            .snapshots
            .iter()
            .find(|snapshot| snapshot.query == query)
        else {
            return false;
        };

        self.matches.clone_from(&snapshot.matches);
        true
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
                self.label_match_indices[usize::from(label as u8)] = Some(idx);
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
        let first_query_byte = query_bytes[0];

        for token in self.tokens.iter().rev() {
            let token_bytes = token.text.as_bytes();
            if query_len > token_bytes.len() {
                continue;
            }

            for match_pos in (0..=token_bytes.len() - query_len).rev() {
                if !token_bytes[match_pos].eq_ignore_ascii_case(&first_query_byte)
                    || !is_utf8_boundary(token_bytes, match_pos)
                    || !is_utf8_boundary(token_bytes, match_pos + query_len)
                    || !ascii_case_insensitive_eq(
                        &token_bytes[match_pos..match_pos + query_len],
                        query_bytes,
                    )
                {
                    continue;
                }

                let candidate = SearchMatch {
                    text: token.text,
                    line: token.line,
                    col: token.col,
                    label: None,
                    match_start: match_pos,
                    match_end: match_pos + query_len,
                };
                self.matches.push(candidate);
            }
        }
    }

    fn refine_matches(&mut self, query_bytes: &[u8], previous_query_len: usize) {
        self.matches.retain(|candidate| {
            let token_bytes = candidate.text.as_bytes();
            let extended_end = candidate.match_start + query_bytes.len();
            if extended_end > token_bytes.len() || !is_utf8_boundary(token_bytes, extended_end) {
                return false;
            }

            token_bytes[candidate.match_start + previous_query_len..extended_end]
                .iter()
                .zip(&query_bytes[previous_query_len..])
                .all(|(token_byte, query_byte)| token_byte.eq_ignore_ascii_case(query_byte))
        });

        for candidate in &mut self.matches {
            candidate.match_end = candidate.match_start + query_bytes.len();
        }
    }
}

fn build_tokens<'a>(lines: &[&'a str]) -> Vec<SearchToken<'a>> {
    let mut tokens = Vec::new();

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

            tokens.push(SearchToken {
                text: &line[token_start..idx],
                line: line_idx,
                col: token_start,
            });
        }
    }

    tokens
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

fn ascii_case_insensitive_eq(left: &[u8], right: &[u8]) -> bool {
    if left.len() != right.len() {
        return false;
    }
    left.iter()
        .zip(right)
        .all(|(a, b)| a.eq_ignore_ascii_case(b))
}

fn assign_labels(matches: &mut [SearchMatch<'_>], query: &[u8], label_chars: &str) {
    let mut query_chars = [false; 256];
    let mut continuation_chars = [false; 256];
    let mut used_labels = [false; 256];
    let mut token_chars = [false; 256];
    let mut candidates = [LabelCandidate { label: 0, lower: 0 }; 256];
    let mut candidates_len = 0usize;
    let mut used_candidates = 0usize;
    let mut cached_token: Option<(*const u8, usize)> = None;

    for byte in query {
        query_chars[usize::from(*byte)] = true;
    }

    for m in matches.iter() {
        if m.match_end < m.text.len() {
            let next = m.text.as_bytes()[m.match_end].to_ascii_lowercase();
            continuation_chars[usize::from(next)] = true;
        }
    }

    for label in label_chars.bytes() {
        let lower = label.to_ascii_lowercase();
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
        if used_candidates == candidates_len {
            continue;
        }

        let token_id = (m.text.as_ptr(), m.text.len());
        if cached_token != Some(token_id) {
            token_chars.fill(false);
            for byte in m.text.bytes() {
                token_chars[usize::from(byte.to_ascii_lowercase())] = true;
            }
            cached_token = Some(token_id);
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
            assert_eq!(left.match_start, right.match_start);
            assert_eq!(left.match_end, right.match_end);
            assert_eq!(left.label, right.label);
        }
    }

    #[test]
    fn search_restores_snapshot_when_query_shrinks_to_previous_prefix() {
        let pane = "alpha alphabet alphanumeric\nbeta alphabet";
        let mut incremental = SearchInterface::new(pane, default_labels());
        let mut fresh = SearchInterface::new(pane, default_labels());

        incremental.search("a");
        incremental.search("al");
        incremental.search("alp");
        let restored_matches: Vec<_> = incremental
            .search("al")
            .iter()
            .map(|m| (m.text, m.line, m.col, m.match_start, m.match_end, m.label))
            .collect();
        let fresh_matches: Vec<_> = fresh
            .search("al")
            .iter()
            .map(|m| (m.text, m.line, m.col, m.match_start, m.match_end, m.label))
            .collect();

        assert_eq!(incremental.snapshots.len(), 2);
        assert_eq!(restored_matches, fresh_matches);
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
