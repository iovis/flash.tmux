use std::collections::HashMap;

use unicode_width::UnicodeWidthStr;

#[must_use]
pub fn visible_length(text: &str) -> usize {
    UnicodeWidthStr::width(strip_ansi_codes(text).as_str())
}

#[must_use]
pub fn strip_ansi_codes(text: &str) -> String {
    let bytes = text.as_bytes();
    let mut out: Vec<u8> = Vec::with_capacity(text.len());
    let mut idx = 0usize;

    while idx < bytes.len() {
        if bytes[idx] == 0x1b {
            let skip = ansi_sequence_len(bytes, idx);
            idx = idx.saturating_add(skip);
            continue;
        }

        out.push(bytes[idx]);
        idx += 1;
    }

    String::from_utf8_lossy(&out).to_string()
}

#[allow(clippy::implicit_hasher)]
pub fn map_position_to_colored(
    colored_text: &str,
    plain_pos: usize,
    cache: &mut HashMap<(usize, usize), usize>,
    line_id: usize,
) -> usize {
    if let Some(v) = cache.get(&(line_id, plain_pos)) {
        return *v;
    }

    let bytes = colored_text.as_bytes();
    let mut colored_idx = 0usize;
    let mut plain_idx = 0usize;

    while plain_idx < plain_pos && colored_idx < bytes.len() {
        if bytes[colored_idx] == 0x1b {
            let skip = ansi_sequence_len(bytes, colored_idx);
            colored_idx = colored_idx.saturating_add(skip);
            continue;
        }

        colored_idx += 1;
        plain_idx += 1;
    }

    cache.insert((line_id, plain_pos), colored_idx);
    colored_idx
}

#[must_use]
pub fn advance_plain_chars(colored_text: &str, count: usize) -> usize {
    let bytes = colored_text.as_bytes();
    let mut colored_idx = 0usize;
    let mut plain_idx = 0usize;

    while plain_idx < count && colored_idx < bytes.len() {
        if bytes[colored_idx] == 0x1b {
            let skip = ansi_sequence_len(bytes, colored_idx);
            colored_idx = colored_idx.saturating_add(skip);
            continue;
        }

        if let Some(ch) = colored_text[colored_idx..].chars().next() {
            colored_idx += ch.len_utf8();
            plain_idx += 1;
        } else {
            break;
        }
    }

    colored_idx
}

#[must_use]
pub fn ansi_sequence_len(bytes: &[u8], start: usize) -> usize {
    if start >= bytes.len() {
        return 0;
    }
    if bytes[start] != 0x1b {
        return 1;
    }
    if start + 1 >= bytes.len() {
        return 1;
    }

    match bytes[start + 1] {
        b'[' => {
            let mut idx = start + 2;
            while idx < bytes.len() {
                let b = bytes[idx];
                if (0x40..=0x7e).contains(&b) {
                    idx += 1;
                    break;
                }
                idx += 1;
            }
            idx.saturating_sub(start).max(1)
        }
        b']' | b'P' | b'^' | b'_' | b'X' => {
            let mut idx = start + 2;
            while idx < bytes.len() {
                if bytes[idx] == 0x07 {
                    return idx + 1 - start;
                }
                if bytes[idx] == 0x1b && idx + 1 < bytes.len() && bytes[idx + 1] == b'\\' {
                    return idx + 2 - start;
                }
                idx += 1;
            }
            bytes.len().saturating_sub(start).max(1)
        }
        b'(' | b')' | b'*' | b'+' | b'-' | b'.' | b'/' => {
            if start + 2 < bytes.len() {
                3
            } else {
                bytes.len().saturating_sub(start).max(1)
            }
        }
        _ => 2,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strip_ansi_codes_removes_sgr() {
        let input = "a\x1b[31mred\x1b[0m!";
        assert_eq!(strip_ansi_codes(input), "ared!");
    }

    #[test]
    fn visible_length_ignores_ansi() {
        let input = "\x1b[1mwide\x1b[0m";
        assert_eq!(visible_length(input), 4);
    }

    #[test]
    fn ansi_sequence_len_sgr() {
        let input = b"\x1b[31m";
        assert_eq!(ansi_sequence_len(input, 0), 5);
    }

    #[test]
    fn advance_plain_chars_skips_ansi() {
        let input = "\x1b[31mab\x1b[0m";
        assert_eq!(advance_plain_chars(input, 1), 6);
        assert_eq!(advance_plain_chars(input, 2), 7);
    }

    #[test]
    fn map_position_to_colored_uses_cache() {
        let input = "\x1b[31mab\x1b[0m";
        let mut cache = HashMap::new();
        let first = map_position_to_colored(input, 2, &mut cache, 0);
        let second = map_position_to_colored(input, 2, &mut cache, 0);
        assert_eq!(first, second);
    }
}
