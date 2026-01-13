use std::collections::HashSet;

const DEFAULT_LABELS: &str = "asdfghjklqwertyuiopzxcvbnm";

#[derive(Clone, Debug)]
pub struct SearchMatch {
    pub text: String,
    pub line: usize,
    pub col: usize,
    pub label: Option<char>,
    pub match_start: usize,
    pub match_end: usize,
}

#[derive(Debug)]
pub struct SearchInterface {
    pub lines: Vec<String>,
    matches: Vec<SearchMatch>,
}

impl SearchInterface {
    pub fn new(pane_content: &str) -> Self {
        let lines = pane_content.split('\n').map(ToString::to_string).collect();
        Self {
            lines,
            matches: Vec::new(),
        }
    }

    pub fn search(&mut self, query: &str) -> Vec<SearchMatch> {
        if query.is_empty() {
            self.matches.clear();
            return Vec::new();
        }

        let mut matches = Vec::new();
        let query_cmp = query.to_ascii_lowercase();
        let query_bytes = query_cmp.as_bytes();
        let query_len = query_bytes.len();

        for (line_idx, line) in self.lines.iter().enumerate() {
            for (token_start, token_end) in find_tokens(line) {
                let token = &line[token_start..token_end];
                let token_bytes = token.as_bytes();
                if query_len > token_bytes.len() {
                    continue;
                }

                for (match_pos, _) in token.char_indices() {
                    if match_pos + query_len > token_bytes.len() {
                        break;
                    }
                    if !ascii_case_insensitive_eq(
                        &token_bytes[match_pos..match_pos + query_len],
                        query_bytes,
                    ) {
                        continue;
                    }

                    let match_end = match_pos + query_len;

                    matches.push(SearchMatch {
                        text: token.to_string(),
                        line: line_idx,
                        col: token_start,
                        label: None,
                        match_start: match_pos,
                        match_end,
                    });
                }
            }
        }

        let mut seen = HashSet::new();
        let mut unique = Vec::new();
        for m in matches {
            let key = (m.line, m.col, m.match_start, m.text.clone());
            if seen.insert(key) {
                unique.push(m);
            }
        }

        unique.sort_by_key(|m| (m.line, m.col, m.match_start));
        unique.reverse();

        assign_labels(&mut unique, query);

        self.matches.clone_from(&unique);
        unique
    }

    pub fn get_match_by_label(&self, label: char) -> Option<&SearchMatch> {
        self.matches.iter().find(|m| m.label == Some(label))
    }

    pub fn first_match(&self) -> Option<&SearchMatch> {
        self.matches.first()
    }

    pub fn get_matches_at_line(&self, line_num: usize) -> Vec<&SearchMatch> {
        self.matches.iter().filter(|m| m.line == line_num).collect()
    }
}

pub fn delete_prev_word(input: &str) -> String {
    let delimiters: HashSet<char> = " \t-_.,;:!?/\\()[]{}".chars().collect();
    let mut chars: Vec<char> = input.chars().collect();

    while let Some(&c) = chars.last() {
        if c.is_whitespace() {
            chars.pop();
        } else {
            break;
        }
    }

    while let Some(&c) = chars.last() {
        if delimiters.contains(&c) {
            chars.pop();
        } else {
            break;
        }
    }

    while let Some(&c) = chars.last() {
        if delimiters.contains(&c) {
            break;
        }
        chars.pop();
    }

    chars.into_iter().collect()
}

pub fn trim_wrapping_token(token: &str, match_start: usize, match_end: usize) -> &str {
    let mut start = 0usize;
    for (idx, ch) in token.char_indices() {
        if idx >= match_start {
            break;
        }
        if is_trimmable_char(ch) {
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
        if is_trimmable_char(ch) {
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

fn is_trimmable_char(ch: char) -> bool {
    matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | '"' | '\'' | '`')
}

fn ascii_case_insensitive_eq(left: &[u8], right: &[u8]) -> bool {
    if left.len() != right.len() {
        return false;
    }
    left.iter()
        .zip(right)
        .all(|(a, b)| a.eq_ignore_ascii_case(b))
}

fn find_tokens(line: &str) -> Vec<(usize, usize)> {
    let mut tokens = Vec::new();
    let mut in_token = false;
    let mut start = 0usize;

    for (idx, ch) in line.char_indices() {
        if ch.is_whitespace() {
            if in_token {
                tokens.push((start, idx));
                in_token = false;
            }
        } else if !in_token {
            start = idx;
            in_token = true;
        }
    }

    if in_token {
        tokens.push((start, line.len()));
    }

    tokens
}

fn assign_labels(matches: &mut [SearchMatch], query: &str) {
    let query_chars: HashSet<char> = query.to_ascii_lowercase().chars().collect();

    let mut continuation_chars = HashSet::new();
    for m in matches.iter() {
        if m.match_end < m.text.len() {
            let next = m.text[m.match_end..].chars().next().unwrap_or('\0');
            continuation_chars.insert(next.to_ascii_lowercase());
        }
    }

    let mut used = HashSet::new();

    for m in matches.iter_mut() {
        let match_chars: HashSet<char> = m.text.to_ascii_lowercase().chars().collect();

        let mut label = None;
        for c in DEFAULT_LABELS.chars() {
            if used.contains(&c) {
                continue;
            }
            let c_cmp = c.to_ascii_lowercase();
            if query_chars.contains(&c_cmp)
                || continuation_chars.contains(&c_cmp)
                || match_chars.contains(&c_cmp)
            {
                continue;
            }
            label = Some(c);
            used.insert(c);
            break;
        }

        m.label = label;
    }
}
