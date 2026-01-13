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
    fn ansi_sequence_len_sgr() {
        let input = b"\x1b[31m";
        assert_eq!(ansi_sequence_len(input, 0), 5);
    }
}
