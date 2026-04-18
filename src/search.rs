#[derive(Clone, Copy, Debug)]
pub struct SearchMatch<'a> {
    pub text: &'a str,
    pub line: usize,
    pub col: usize,
    pub label: Option<char>,
    pub match_start: usize,
    pub match_end: usize,
}

#[derive(Debug)]
pub struct SearchInterface<'a> {
    pub lines: Vec<&'a str>,
    matches: Vec<SearchMatch<'a>>,
    label_chars: String,
}

impl<'a> SearchInterface<'a> {
    pub fn new(pane_content: &'a str, label_chars: String) -> Self {
        let lines = pane_content.split('\n').collect();
        Self {
            lines,
            matches: Vec::new(),
            label_chars,
        }
    }

    pub fn search(&mut self, query: &str) -> &[SearchMatch<'a>] {
        self.matches.clear();
        if query.is_empty() {
            return &self.matches;
        }

        let query_cmp = query.to_ascii_lowercase();
        let query_bytes = query_cmp.as_bytes();
        let query_len = query_bytes.len();

        for (line_idx, line) in self.lines.iter().copied().enumerate() {
            let mut token_start = 0usize;
            let mut in_token = false;

            for (idx, ch) in line
                .char_indices()
                .chain(std::iter::once((line.len(), '\n')))
            {
                let in_current_token = idx < line.len() && !ch.is_whitespace();

                if in_current_token && !in_token {
                    token_start = idx;
                    in_token = true;
                }

                if !in_current_token && in_token {
                    let token = &line[token_start..idx];
                    let token_bytes = token.as_bytes();
                    if query_len <= token_bytes.len() {
                        for match_pos in 0..=token_bytes.len() - query_len {
                            if !token.is_char_boundary(match_pos)
                                || !token.is_char_boundary(match_pos + query_len)
                                || !ascii_case_insensitive_eq(
                                    &token_bytes[match_pos..match_pos + query_len],
                                    query_bytes,
                                )
                            {
                                continue;
                            }

                            let candidate = SearchMatch {
                                text: token,
                                line: line_idx,
                                col: token_start,
                                label: None,
                                match_start: match_pos,
                                match_end: match_pos + query_len,
                            };
                            self.matches.push(candidate);
                        }
                    }

                    in_token = false;
                }
            }
        }

        self.matches.sort_unstable_by(|left, right| {
            right
                .line
                .cmp(&left.line)
                .then_with(|| right.col.cmp(&left.col))
                .then_with(|| right.match_start.cmp(&left.match_start))
        });
        assign_labels(&mut self.matches, query, &self.label_chars);

        &self.matches
    }

    pub fn get_match_by_label(&self, label: char) -> Option<&SearchMatch<'a>> {
        self.matches.iter().find(|m| m.label == Some(label))
    }

    pub fn first_visible_match(&self, max_lines: usize) -> Option<&SearchMatch<'a>> {
        self.matches.iter().find(|m| m.line < max_lines)
    }

    pub fn get_matches_at_line(&self, line_num: usize) -> Vec<&SearchMatch<'a>> {
        self.matches.iter().filter(|m| m.line == line_num).collect()
    }
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

fn ascii_case_insensitive_eq(left: &[u8], right: &[u8]) -> bool {
    if left.len() != right.len() {
        return false;
    }
    left.iter()
        .zip(right)
        .all(|(a, b)| a.eq_ignore_ascii_case(b))
}

fn assign_labels(matches: &mut [SearchMatch<'_>], query: &str, label_chars: &str) {
    let mut query_chars = [false; 256];
    let mut continuation_chars = [false; 256];
    let mut used_labels = [false; 256];

    for byte in query.bytes() {
        query_chars[usize::from(byte.to_ascii_lowercase())] = true;
    }

    for m in matches.iter() {
        if m.match_end < m.text.len() {
            let next = m.text.as_bytes()[m.match_end].to_ascii_lowercase();
            continuation_chars[usize::from(next)] = true;
        }
    }

    for m in matches.iter_mut() {
        let mut token_chars = [false; 256];
        for byte in m.text.bytes() {
            token_chars[usize::from(byte.to_ascii_lowercase())] = true;
        }

        m.label = None;
        for label in label_chars.bytes() {
            let lower = label.to_ascii_lowercase();
            if used_labels[usize::from(label)]
                || query_chars[usize::from(lower)]
                || continuation_chars[usize::from(lower)]
                || token_chars[usize::from(lower)]
            {
                continue;
            }

            m.label = Some(char::from(label));
            used_labels[usize::from(label)] = true;
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
    fn search_ordering_is_reverse() {
        let mut search = SearchInterface::new("abc abc", default_labels());
        let matches = search.search("a");
        assert_eq!(matches.len(), 2);
        assert_eq!(matches[0].col, 4);
        assert_eq!(matches[1].col, 0);
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
