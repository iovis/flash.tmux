use anyhow::{Context, Result};
use crossterm::cursor::MoveTo;
use crossterm::event::{Event, KeyCode, KeyEventKind, KeyModifiers};
use crossterm::style::{self, Attribute, Stylize};
use crossterm::terminal::{self, Clear, ClearType};
use crossterm::{QueueableCommand, execute};
use std::io::{self, IsTerminal, Write};
use unicode_width::UnicodeWidthStr;

use crate::config::Config;
use crate::search::{SearchInterface, SearchMatch, delete_prev_word, trim_wrapping_token};
use crate::tmux::{ExitAction, tmux_run_quiet};

pub struct InteractiveUI {
    pane_id: String,
    config: Config,
    search: SearchInterface,
    search_query: String,
}

impl InteractiveUI {
    #[must_use]
    pub fn new(pane_id: String, pane_content: &str, config: Config) -> Self {
        let search = SearchInterface::new(pane_content);

        Self {
            pane_id,
            config,
            search,
            search_query: String::new(),
        }
    }

    pub fn run(&mut self) -> Result<()> {
        let _term_guard = TerminalModeGuard::new()?;

        self.display_content()?;

        loop {
            match crossterm::event::read()? {
                Event::Key(key) if matches!(key.kind, KeyEventKind::Release) => {}
                Event::Resize(_, _) => self.display_content()?,
                Event::Key(key) => {
                    let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
                    match key.code {
                        KeyCode::Char('c' | 'd') if ctrl => {
                            self.save_result("", ExitAction::Cancel)?;
                            return Ok(());
                        }
                        KeyCode::Esc => {
                            self.save_result("", ExitAction::Cancel)?;
                            return Ok(());
                        }
                        KeyCode::Char('u') if ctrl => {
                            self.update_search(String::new())?;
                        }
                        KeyCode::Char('w') if ctrl => {
                            let new_query = delete_prev_word(&self.search_query);
                            self.update_search(new_query)?;
                        }
                        KeyCode::Backspace => {
                            if !self.search_query.is_empty() {
                                let mut new_query = self.search_query.clone();
                                new_query.pop();
                                self.update_search(new_query)?;
                            }
                        }
                        KeyCode::Enter | KeyCode::Char(' ') if !ctrl => {
                            if let Some(first) = self.search.first_match() {
                                let text = trim_wrapping_token(
                                    &first.text,
                                    first.match_start,
                                    first.match_end,
                                );
                                let action = if matches!(key.code, KeyCode::Enter) {
                                    ExitAction::PasteAndEnter
                                } else {
                                    ExitAction::PasteAndSpace
                                };
                                self.save_result(text, action)?;
                                return Ok(());
                            }
                        }
                        KeyCode::Char(c) if !ctrl => {
                            let label_lookup = c.to_ascii_lowercase();
                            if !self.search_query.is_empty()
                                && let Some(match_item) =
                                    self.search.get_match_by_label(label_lookup)
                            {
                                let action = if c.is_ascii_lowercase() {
                                    ExitAction::Paste
                                } else {
                                    ExitAction::CopyOnly
                                };
                                let text = trim_wrapping_token(
                                    &match_item.text,
                                    match_item.match_start,
                                    match_item.match_end,
                                );
                                self.save_result(text, action)?;
                                return Ok(());
                            }

                            if c.is_ascii_graphic() || c == ' ' {
                                let mut new_query = self.search_query.clone();
                                new_query.push(c);
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
        self.search.search(&self.search_query);
        self.display_content()
    }

    fn display_content(&self) -> Result<()> {
        let mut out = io::stderr();
        execute!(out, Clear(ClearType::All), MoveTo(0, 0))?;

        let (_, height) = terminal::size().unwrap_or((80, 40));
        let height = height as usize;

        let available_height = height.saturating_sub(1);

        out.queue(MoveTo(0, 0))?;

        self.display_pane_content(&mut out, available_height)?;

        let prompt = self.build_search_bar_output();
        let prompt_row = u16::try_from(height.saturating_sub(1)).unwrap_or(u16::MAX);
        out.queue(MoveTo(0, prompt_row))?;
        out.write_all(prompt.as_bytes())?;

        let cursor_col =
            u16::try_from(self.prompt_cursor_column().saturating_sub(1)).unwrap_or(u16::MAX);
        out.queue(MoveTo(cursor_col, prompt_row))?;

        out.flush()?;
        Ok(())
    }

    fn prompt_cursor_column(&self) -> usize {
        let mut col = UnicodeWidthStr::width(self.config.prompt_indicator.as_str()) + 2;
        if !self.search_query.is_empty() {
            col += UnicodeWidthStr::width(self.search_query.as_str());
        }
        col
    }

    fn display_pane_content(&self, out: &mut io::Stderr, available_height: usize) -> Result<()> {
        let total_lines = self.search.lines.len().min(available_height);

        for (line_idx, line) in self.search.lines.iter().take(available_height).enumerate() {
            let matches = self.search.get_matches_at_line(line_idx);
            let current_match = self
                .search
                .first_match()
                .filter(|m| m.line == line_idx)
                .map(|m| (m.col, m.match_start, m.match_end));
            let is_last_line = line_idx + 1 == total_lines;

            let output = render_line_with_matches(line, &matches, &self.config, current_match);

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
            base.push_str(&dim_text(&self.config.prompt_placeholder_text));
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

    fn save_result(&self, text: &str, action: ExitAction) -> Result<()> {
        let pane_id = &self.pane_id;
        let buffer = format!("__flash_copy_result_{pane_id}__");
        let _ = tmux_run_quiet(&["set-buffer", "-b", &buffer, "--", text]);
        std::process::exit(action.exit_code());
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
        let _ = execute!(out, Clear(ClearType::All), MoveTo(0, 0));
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

fn dim_text(text: &str) -> String {
    format!("{}", style::style(text).attribute(Attribute::Dim))
}

fn render_line_with_matches(
    line: &str,
    matches: &[&SearchMatch],
    config: &Config,
    current_match: Option<(usize, usize, usize)>,
) -> String {
    if line.is_empty() {
        return format!(
            "{}{}",
            config.style_sequences.dim, config.style_sequences.reset
        );
    }
    if matches.is_empty() {
        return format!(
            "{}{}{}",
            config.style_sequences.dim, line, config.style_sequences.reset
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
        let style_kind = if current_match == Some((m.col, m.match_start, m.match_end)) {
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
    out.push_str(&config.style_sequences.dim);

    let mut active = StyleKind::Base;
    let mut buffer = String::new();

    let mut label_iter = label_positions.iter().peekable();
    for (idx, ch) in line.char_indices() {
        if let Some((_, label)) = label_iter.next_if(|(pos, _)| *pos == idx) {
            flush_segment(&mut out, &mut buffer, active, config);
            out.push_str(&config.style_sequences.reset);
            out.push_str(&config.label_style.apply(&label.to_string()));
            out.push_str(&config.style_sequences.reset);
            out.push_str(&config.style_sequences.dim);
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
        out.push_str(&config.style_sequences.dim);
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
            out.push_str(&config.style_sequences.dim);
        }
        StyleKind::Current => {
            out.push_str(&config.style_sequences.reset);
            out.push_str(&config.current_style.apply(buffer));
            out.push_str(&config.style_sequences.reset);
            out.push_str(&config.style_sequences.dim);
        }
    }

    buffer.clear();
}
