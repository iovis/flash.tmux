use anyhow::{Context, Result};
use crossterm::cursor::{Hide, MoveTo, Show};
use crossterm::event::{Event, KeyCode, KeyEventKind, KeyModifiers};
use crossterm::terminal::{self, Clear, ClearType};
use crossterm::{QueueableCommand, SynchronizedUpdate, execute};
use std::io::{self, IsTerminal, Write};
use unicode_width::UnicodeWidthStr;

use crate::config::{Config, LabelActionMode};
use crate::search::{SearchInterface, SearchMatch, delete_prev_word, trim_wrapping_token};
use crate::tmux::ExitAction;

pub struct InteractiveUI<'a> {
    config: Config,
    search: SearchInterface<'a>,
    search_query: String,
    cursor_pos: usize,
}

pub struct SelectedText {
    pub text: String,
    pub action: ExitAction,
}

impl SelectedText {
    fn new(text: impl Into<String>, action: ExitAction) -> Self {
        Self {
            text: text.into(),
            action,
        }
    }
}

impl<'a> InteractiveUI<'a> {
    pub fn new(pane_content: &'a str, config: Config) -> Self {
        let label_chars = config.label_characters.clone();
        let search = SearchInterface::new(pane_content, label_chars);

        Self {
            config,
            search,
            search_query: String::new(),
            cursor_pos: 0,
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn run(&mut self) -> Result<SelectedText> {
        let _term_guard = TerminalModeGuard::new()?;

        self.display_content()?;

        loop {
            match crossterm::event::read()? {
                Event::Key(key) if matches!(key.kind, KeyEventKind::Release) => {}
                Event::Resize(_, _) => self.display_content()?,
                Event::Key(key) => {
                    let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
                    match (key.code, ctrl) {
                        (KeyCode::Char('c' | 'd'), true) | (KeyCode::Esc, _) => {
                            return Ok(SelectedText::new("", ExitAction::Cancel));
                        }
                        (KeyCode::Char('a'), true) | (KeyCode::Home, _) => {
                            self.cursor_pos = 0;
                            self.display_content()?;
                        }
                        (KeyCode::Char('e'), true) | (KeyCode::End, _) => {
                            self.cursor_pos = self.search_query.len();
                            self.display_content()?;
                        }
                        (KeyCode::Left, _) => {
                            self.cursor_pos = self.cursor_pos.saturating_sub(1);
                            self.display_content()?;
                        }
                        (KeyCode::Right, _) if self.cursor_pos < self.search_query.len() => {
                            self.cursor_pos += 1;
                            self.display_content()?;
                        }
                        (KeyCode::Char('u'), true) => {
                            self.cursor_pos = 0;
                            self.update_search(String::new())?;
                        }
                        (KeyCode::Char('w'), true) => {
                            let (head, tail) = self.search_query.split_at(self.cursor_pos);
                            let new_head = delete_prev_word(head);
                            let new_cursor = new_head.len();
                            let new_query = format!("{new_head}{tail}");
                            self.cursor_pos = new_cursor;
                            self.update_search(new_query)?;
                        }
                        (KeyCode::Backspace, _) if self.cursor_pos > 0 => {
                            let mut new_query = self.search_query.clone();
                            new_query.remove(self.cursor_pos - 1);
                            self.cursor_pos = self.cursor_pos.saturating_sub(1);
                            self.update_search(new_query)?;
                        }
                        (KeyCode::Enter | KeyCode::Char(' ') | KeyCode::Tab, false) => {
                            let max_lines = Self::visible_line_limit();
                            if let Some(first) = self.search.first_visible_match(max_lines) {
                                let text = trim_wrapping_token(
                                    first.text,
                                    first.match_start,
                                    first.match_end,
                                    &self.config.trimmable_chars,
                                );
                                let action = match key.code {
                                    KeyCode::Enter => ExitAction::PasteAndEnter,
                                    KeyCode::Char(' ') => ExitAction::PasteAndSpace,
                                    _ => ExitAction::Paste,
                                };
                                return Ok(SelectedText::new(text, action));
                            }
                        }
                        (KeyCode::Char(c), false) => {
                            let label_lookup = c.to_ascii_lowercase();
                            if !self.search_query.is_empty()
                                && let Some(match_item) =
                                    self.search.get_match_by_label(label_lookup)
                            {
                                let action = label_key_action(c, self.config.label_action_mode);
                                let text = trim_wrapping_token(
                                    match_item.text,
                                    match_item.match_start,
                                    match_item.match_end,
                                    &self.config.trimmable_chars,
                                );
                                return Ok(SelectedText::new(text, action));
                            }

                            if c.is_ascii_graphic() || c == ' ' {
                                let mut new_query = self.search_query.clone();
                                new_query.insert(self.cursor_pos, c);
                                self.cursor_pos += 1;
                                self.update_search(new_query)?;
                            }
                        }
                        _ => {}
                    }
                }
                Event::Mouse(_) | Event::Paste(_) | Event::FocusGained | Event::FocusLost => {}
            }
        }
    }

    fn update_search(&mut self, new_query: String) -> Result<()> {
        self.search_query = new_query;
        self.cursor_pos = self.cursor_pos.min(self.search_query.len());
        self.search.search(&self.search_query);
        self.display_content()
    }

    fn display_content(&self) -> Result<()> {
        let mut out = io::stderr();
        let available_height = Self::visible_line_limit();
        let height = available_height.saturating_add(1);

        out.sync_update(|out| -> io::Result<()> {
            out.queue(Hide)?;
            out.queue(MoveTo(0, 0))?;
            out.queue(Clear(ClearType::FromCursorDown))?;

            self.display_pane_content(out, available_height)
                .map_err(io::Error::other)?;

            let prompt = self.build_search_bar_output();
            let prompt_row = u16::try_from(height.saturating_sub(1)).unwrap_or(u16::MAX);
            out.queue(MoveTo(0, prompt_row))?;
            out.write_all(prompt.as_bytes())?;

            let cursor_col =
                u16::try_from(self.prompt_cursor_column().saturating_sub(1)).unwrap_or(u16::MAX);
            out.queue(MoveTo(cursor_col, prompt_row))?;
            out.queue(Show)?;

            Ok(())
        })??;
        Ok(())
    }

    fn visible_line_limit() -> usize {
        let (_, height) = terminal::size().unwrap_or((80, 40));
        (height as usize).saturating_sub(1)
    }

    fn prompt_cursor_column(&self) -> usize {
        let mut col = UnicodeWidthStr::width(self.config.prompt_indicator.as_str()) + 2;
        if self.cursor_pos > 0 {
            let cursor_slice = &self.search_query[..self.cursor_pos];
            col += UnicodeWidthStr::width(cursor_slice);
        }
        col
    }

    fn display_pane_content(&self, out: &mut io::Stderr, available_height: usize) -> Result<()> {
        let total_lines = self.search.lines.len().min(available_height);
        let first_visible = self
            .search
            .first_visible_match(total_lines)
            .map(|m| (m.line, m.col, m.match_start, m.match_end));

        for (line_idx, line) in self.search.lines.iter().take(available_height).enumerate() {
            let matches = self.search.get_matches_at_line(line_idx);
            let current_match = first_visible.filter(|m| m.0 == line_idx);
            let is_last_line = line_idx + 1 == total_lines;

            let output = render_line_with_matches(line, matches, &self.config, current_match);

            if is_last_line {
                out.write_all(output.as_bytes())?;
            } else {
                out.write_all(output.as_bytes())?;
                out.write_all(b"\r\n")?;
            }
        }

        Ok(())
    }

    fn build_search_bar_output(&self) -> String {
        let mut base = String::new();
        if self.search_query.is_empty() {
            base.push_str(
                &self
                    .config
                    .prompt_style
                    .apply(&self.config.prompt_indicator),
            );
            base.push(' ');
            base.push_str(&base_text(
                &self.config.prompt_placeholder_text,
                &self.config,
            ));
        } else {
            base.push_str(
                &self
                    .config
                    .prompt_style
                    .apply(&self.config.prompt_indicator),
            );
            base.push(' ');
            base.push_str(&self.search_query);
        }

        base
    }
}

struct TerminalModeGuard {
    raw_mode_enabled: bool,
}

impl TerminalModeGuard {
    fn new() -> Result<Self> {
        if !io::stdin().is_terminal() {
            return Ok(Self {
                raw_mode_enabled: false,
            });
        }

        terminal::enable_raw_mode().context("failed to enable raw mode")?;
        Ok(Self {
            raw_mode_enabled: true,
        })
    }
}

impl Drop for TerminalModeGuard {
    fn drop(&mut self) {
        if self.raw_mode_enabled {
            let _ = terminal::disable_raw_mode();
        }
        let mut out = io::stderr();
        let _ = execute!(out, Show, Clear(ClearType::All), MoveTo(0, 0));
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum StyleKind {
    Base,
    Highlight,
    Current,
}

impl StyleKind {
    fn priority(self) -> u8 {
        match self {
            StyleKind::Base => 0,
            StyleKind::Highlight => 1,
            StyleKind::Current => 2,
        }
    }
}

fn base_text(text: &str, config: &Config) -> String {
    config.base_style.apply(text)
}

fn label_key_action(key: char, label_action_mode: LabelActionMode) -> ExitAction {
    let should_paste = match label_action_mode {
        LabelActionMode::Default => key.is_ascii_lowercase(),
        LabelActionMode::Reversed => key.is_ascii_uppercase(),
    };

    if should_paste {
        ExitAction::Paste
    } else {
        ExitAction::CopyOnly
    }
}

fn render_line_with_matches(
    line: &str,
    matches: &[SearchMatch<'_>],
    config: &Config,
    current_match: Option<(usize, usize, usize, usize)>,
) -> String {
    if line.is_empty() {
        return format!(
            "{}{}",
            config.style_sequences.base, config.style_sequences.reset
        );
    }
    if matches.is_empty() {
        return format!(
            "{}{}{}",
            config.style_sequences.base, line, config.style_sequences.reset
        );
    }

    let mut label_positions: Vec<(usize, char)> = matches
        .iter()
        .filter_map(|m| m.label.map(|label| (m.col + m.match_end, label)))
        .filter(|(pos, _)| *pos <= line.len())
        .collect();
    label_positions.sort_by_key(|(pos, _)| *pos);
    label_positions.dedup_by_key(|(pos, _)| *pos);

    let mut style_map = vec![StyleKind::Base; line.len()];
    for m in matches {
        let style_kind = if current_match == Some((m.line, m.col, m.match_start, m.match_end)) {
            StyleKind::Current
        } else {
            StyleKind::Highlight
        };
        let start = m.col + m.match_start;
        let end = m.col + m.match_end;
        if start >= end {
            continue;
        }
        let start = start.min(line.len());
        let end = end.min(line.len());
        for slot in style_map.iter_mut().take(end).skip(start) {
            if style_kind.priority() > slot.priority() {
                *slot = style_kind;
            }
        }
    }

    let mut out = String::new();
    out.push_str(&config.style_sequences.base);

    let mut active = StyleKind::Base;
    let mut buffer = String::new();

    let mut label_iter = label_positions.iter().peekable();
    for (idx, ch) in line.char_indices() {
        if let Some((_, label)) = label_iter.next_if(|(pos, _)| *pos == idx) {
            flush_segment(&mut out, &mut buffer, active, config);
            out.push_str(&config.style_sequences.reset);
            out.push_str(&config.label_style.apply(&label.to_string()));
            out.push_str(&config.style_sequences.reset);
            out.push_str(&config.style_sequences.base);
            continue;
        }

        let style_kind = style_map.get(idx).copied().unwrap_or(StyleKind::Base);
        if style_kind != active {
            flush_segment(&mut out, &mut buffer, active, config);
            active = style_kind;
        }

        buffer.push(ch);
    }

    if let Some((_, label)) = label_iter.next_if(|(pos, _)| *pos == line.len()) {
        flush_segment(&mut out, &mut buffer, active, config);
        out.push_str(&config.style_sequences.reset);
        out.push_str(&config.label_style.apply(&label.to_string()));
        out.push_str(&config.style_sequences.reset);
        out.push_str(&config.style_sequences.base);
    }

    flush_segment(&mut out, &mut buffer, active, config);

    if !out.ends_with(&config.style_sequences.reset) {
        out.push_str(&config.style_sequences.reset);
    }

    out
}

fn flush_segment(out: &mut String, buffer: &mut String, style: StyleKind, config: &Config) {
    if buffer.is_empty() {
        return;
    }

    match style {
        StyleKind::Base => out.push_str(buffer),
        StyleKind::Highlight => {
            out.push_str(&config.style_sequences.reset);
            out.push_str(&config.highlight_style.apply(buffer));
            out.push_str(&config.style_sequences.reset);
            out.push_str(&config.style_sequences.base);
        }
        StyleKind::Current => {
            out.push_str(&config.style_sequences.reset);
            out.push_str(&config.current_style.apply(buffer));
            out.push_str(&config.style_sequences.reset);
            out.push_str(&config.style_sequences.base);
        }
    }

    buffer.clear();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use crate::search::SearchMatch;

    fn cfg() -> Config {
        Config::defaults()
    }

    /// Helper: build the expected string for base-styled text (no matches).
    fn base(text: &str, config: &Config) -> String {
        format!(
            "{}{}{}",
            config.style_sequences.base, text, config.style_sequences.reset
        )
    }

    /// Helper: wrap text in highlight style with surrounding reset/base.
    fn highlight(text: &str, config: &Config) -> String {
        format!(
            "{}{}{}{}",
            config.style_sequences.reset,
            config.highlight_style.apply(text),
            config.style_sequences.reset,
            config.style_sequences.base,
        )
    }

    /// Helper: wrap text in current-match style with surrounding reset/base.
    fn current_style(text: &str, config: &Config) -> String {
        format!(
            "{}{}{}{}",
            config.style_sequences.reset,
            config.current_style.apply(text),
            config.style_sequences.reset,
            config.style_sequences.base,
        )
    }

    /// Helper: wrap a label character in label style with surrounding reset/base.
    fn label(ch: char, config: &Config) -> String {
        format!(
            "{}{}{}{}",
            config.style_sequences.reset,
            config.label_style.apply(&ch.to_string()),
            config.style_sequences.reset,
            config.style_sequences.base,
        )
    }

    fn make_match(
        text: &'static str,
        line: usize,
        col: usize,
        match_start: usize,
        match_end: usize,
        lbl: Option<char>,
    ) -> SearchMatch<'static> {
        SearchMatch {
            text,
            line,
            col,
            label: lbl,
            match_start,
            match_end,
        }
    }

    #[test]
    fn lowercase_label_pastes_by_default() {
        assert!(matches!(
            label_key_action('a', LabelActionMode::Default),
            ExitAction::Paste
        ));
    }

    #[test]
    fn uppercase_label_copies_by_default() {
        assert!(matches!(
            label_key_action('A', LabelActionMode::Default),
            ExitAction::CopyOnly
        ));
    }

    #[test]
    fn lowercase_label_copies_when_reversed() {
        assert!(matches!(
            label_key_action('a', LabelActionMode::Reversed),
            ExitAction::CopyOnly
        ));
    }

    #[test]
    fn uppercase_label_pastes_when_reversed() {
        assert!(matches!(
            label_key_action('A', LabelActionMode::Reversed),
            ExitAction::Paste
        ));
    }

    // --- empty / no-match cases ---

    #[test]
    fn empty_line_no_matches() {
        let c = cfg();
        let result = render_line_with_matches("", &[], &c, None);
        let expected = format!("{}{}", c.style_sequences.base, c.style_sequences.reset);
        assert_eq!(result, expected);
    }

    #[test]
    fn non_empty_line_no_matches() {
        let c = cfg();
        let result = render_line_with_matches("hello world", &[], &c, None);
        assert_eq!(result, base("hello world", &c));
    }

    // --- single match ---

    #[test]
    fn single_highlight_match() {
        let c = cfg();
        // line: "foo bar", match on "foo" (col=0, match_start=0, match_end=3), no label
        let m = make_match("foo", 0, 0, 0, 3, None);
        let matches = [m];
        let result = render_line_with_matches("foo bar", &matches, &c, None);

        let expected = format!(
            "{}{}{}{}",
            c.style_sequences.base,
            highlight("foo", &c),
            " bar",
            c.style_sequences.reset,
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn single_current_match() {
        let c = cfg();
        // "foo bar", match on "foo", marked as current
        let m = make_match("foo", 0, 0, 0, 3, None);
        let matches = [m];
        let current = Some((0, 0, 0, 3));
        let result = render_line_with_matches("foo bar", &matches, &c, current);

        let expected = format!(
            "{}{}{}{}",
            c.style_sequences.base,
            current_style("foo", &c),
            " bar",
            c.style_sequences.reset,
        );
        assert_eq!(result, expected);
    }

    // --- labels ---

    #[test]
    fn label_inserted_after_match() {
        let c = cfg();
        // "foo bar", match "foo" with label 'a'. Label appears at byte 3 (right after "foo").
        let m = make_match("foo", 0, 0, 0, 3, Some('a'));
        let matches = [m];
        let result = render_line_with_matches("foo bar", &matches, &c, None);

        // "foo" highlighted, then label 'a' replaces the space, then "bar" as base
        let expected = format!(
            "{}{}{}{}{}",
            c.style_sequences.base,
            highlight("foo", &c),
            label('a', &c),
            "bar",
            c.style_sequences.reset,
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn label_at_end_of_line() {
        let c = cfg();
        // "foo", match "foo" (entire token), label 'a'. Label at position 3 == line.len().
        let m = make_match("foo", 0, 0, 0, 3, Some('a'));
        let matches = [m];
        let result = render_line_with_matches("foo", &matches, &c, None);

        let expected = format!(
            "{}{}{}{}",
            c.style_sequences.base,
            highlight("foo", &c),
            label('a', &c),
            c.style_sequences.reset,
        );
        assert_eq!(result, expected);
    }

    // --- match in the middle of a line ---

    #[test]
    fn match_in_middle_of_line() {
        let c = cfg();
        // "hello world end", match on "world" at col=6, match_start=0, match_end=5
        let m = make_match("world", 0, 6, 0, 5, None);
        let matches = [m];
        let result = render_line_with_matches("hello world end", &matches, &c, None);

        let expected = format!(
            "{}{}{}{}{}",
            c.style_sequences.base,
            "hello ",
            highlight("world", &c),
            " end",
            c.style_sequences.reset,
        );
        assert_eq!(result, expected);
    }

    // --- multiple matches on the same line ---

    #[test]
    fn multiple_matches_same_line() {
        let c = cfg();
        // "foo bar foo", two "foo" tokens: col=0 and col=8
        let m1 = make_match("foo", 0, 0, 0, 3, Some('a'));
        let m2 = make_match("foo", 0, 8, 0, 3, Some('s'));
        let matches = [m1, m2];
        let result = render_line_with_matches("foo bar foo", &matches, &c, None);

        let expected = format!(
            "{}{}{}{}{}{}{}",
            c.style_sequences.base,
            highlight("foo", &c),
            label('a', &c),
            "bar ",
            highlight("foo", &c),
            label('s', &c),
            c.style_sequences.reset,
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn one_current_one_highlight() {
        let c = cfg();
        // "foo bar foo", first match is current, second is highlight
        let m1 = make_match("foo", 0, 0, 0, 3, Some('a'));
        let m2 = make_match("foo", 0, 8, 0, 3, Some('s'));
        let matches = [m1, m2];
        let current = Some((0, 0, 0, 3)); // m1 is current
        let result = render_line_with_matches("foo bar foo", &matches, &c, current);

        let expected = format!(
            "{}{}{}{}{}{}{}",
            c.style_sequences.base,
            current_style("foo", &c),
            label('a', &c),
            "bar ",
            highlight("foo", &c),
            label('s', &c),
            c.style_sequences.reset,
        );
        assert_eq!(result, expected);
    }

    // --- match without label ---

    #[test]
    fn match_without_label() {
        let c = cfg();
        // match on "bar" with no label assigned
        let m = make_match("bar", 0, 4, 0, 3, None);
        let matches = [m];
        let result = render_line_with_matches("foo bar baz", &matches, &c, None);

        let expected = format!(
            "{}{}{}{}{}",
            c.style_sequences.base,
            "foo ",
            highlight("bar", &c),
            " baz",
            c.style_sequences.reset,
        );
        assert_eq!(result, expected);
    }

    // --- partial match within a token ---

    #[test]
    fn partial_match_within_token() {
        let c = cfg();
        // "foobar", match on "oo" within "foobar" (col=0, match_start=1, match_end=3)
        let m = make_match("foobar", 0, 0, 1, 3, Some('a'));
        let matches = [m];
        let result = render_line_with_matches("foobar", &matches, &c, None);

        // "f" base, "oo" highlighted, label 'a' replaces 'b', "ar" base
        let expected = format!(
            "{}{}{}{}{}{}",
            c.style_sequences.base,
            "f",
            highlight("oo", &c),
            label('a', &c),
            "ar",
            c.style_sequences.reset,
        );
        assert_eq!(result, expected);
    }

    // --- current match takes priority over highlight ---

    #[test]
    fn current_priority_over_highlight() {
        let c = cfg();
        // Two matches at the same position: if one is current, current style wins.
        // This can happen with overlapping matches from different query tokens.
        // "abcabc", match1: col=0 match_start=0 match_end=3 (highlight)
        //           match2: col=0 match_start=1 match_end=3 (current)
        // Bytes 0 = highlight, bytes 1-2 = current (higher priority)
        let m1 = make_match("abcabc", 0, 0, 0, 3, None);
        let m2 = make_match("abcabc", 0, 0, 1, 3, None);
        let matches = [m1, m2];
        let current = Some((0, 0, 1, 3)); // m2 is current
        let result = render_line_with_matches("abcabc", &matches, &c, current);

        let expected = format!(
            "{}{}{}{}{}",
            c.style_sequences.base,
            highlight("a", &c),
            current_style("bc", &c),
            "abc",
            c.style_sequences.reset,
        );
        assert_eq!(result, expected);
    }

    // --- label deduplication at same position ---

    #[test]
    fn duplicate_label_positions_deduplicated() {
        let c = cfg();
        // Two matches that would place labels at the same position.
        // Both end at col+match_end = 3. Only one label should appear.
        let m1 = make_match("foo", 0, 0, 0, 3, Some('a'));
        let m2 = make_match("foo", 0, 0, 0, 3, Some('s'));
        let matches = [m1, m2];
        let result = render_line_with_matches("foo bar", &matches, &c, None);

        // Both matches highlight "foo", but only one label at position 3.
        // label_positions dedup keeps the first one ('a').
        let expected = format!(
            "{}{}{}{}{}",
            c.style_sequences.base,
            highlight("foo", &c),
            label('a', &c),
            "bar",
            c.style_sequences.reset,
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn same_position_different_line_not_marked_current() {
        let c = cfg();
        let m = make_match("foo", 1, 0, 0, 3, None);
        let matches = [m];
        let current = Some((0, 0, 0, 3));
        let result = render_line_with_matches("foo", &matches, &c, current);

        let expected = format!(
            "{}{}{}",
            c.style_sequences.base,
            highlight("foo", &c),
            c.style_sequences.reset,
        );
        assert_eq!(result, expected);
    }

    // --- build_search_bar_output ---

    #[test]
    fn search_bar_empty_query() {
        let c = cfg();
        let ui = InteractiveUI {
            search: SearchInterface::new("", c.label_characters.clone()),
            search_query: String::new(),
            cursor_pos: 0,
            config: c.clone(),
        };

        let result = ui.build_search_bar_output();
        let expected = format!(
            "{} {}",
            c.prompt_style.apply(&c.prompt_indicator),
            c.base_style.apply(&c.prompt_placeholder_text),
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn search_bar_with_query() {
        let c = cfg();
        let ui = InteractiveUI {
            search: SearchInterface::new("", c.label_characters.clone()),
            search_query: "hello".to_string(),
            cursor_pos: 5,
            config: c.clone(),
        };

        let result = ui.build_search_bar_output();
        let expected = format!("{} hello", c.prompt_style.apply(&c.prompt_indicator),);
        assert_eq!(result, expected);
    }
}
