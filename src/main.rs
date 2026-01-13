use anyhow::{Context, Result, bail};
use clap::Parser;
use crossterm::cursor::MoveTo;
use crossterm::event::{Event, KeyCode, KeyEventKind, KeyModifiers};
use crossterm::style::{self, Attribute, Color, SetAttribute, Stylize};
use crossterm::terminal::{self, Clear, ClearType};
use crossterm::{QueueableCommand, execute};
use std::collections::HashSet;
use std::io::{self, IsTerminal, Write};
use std::process::Command;
use unicode_width::UnicodeWidthStr;

const DEFAULT_LABELS: &str = "asdfghjklqwertyuiopzxcvbnm";

const EXIT_CODE_PASTE: i32 = 10;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    #[arg(long)]
    interactive: bool,
    #[arg(long)]
    pane_id: Option<String>,
}

#[derive(Clone)]
struct Config {
    prompt_placeholder_text: String,
    prompt_indicator: String,
    highlight_style: StyleSpec,
    current_style: StyleSpec,
    label_style: StyleSpec,
    prompt_style: StyleSpec,
    style_sequences: StyleSequences,
}

impl Config {
    fn defaults() -> Self {
        Self {
            prompt_placeholder_text: "search...".to_string(),
            prompt_indicator: "❯".to_string(),
            highlight_style: StyleSpec::new(Some(Color::Rgb {
                r: 186,
                g: 187,
                b: 242,
            })),
            current_style: StyleSpec::new(Some(Color::Rgb {
                r: 239,
                g: 159,
                b: 119,
            })),
            label_style: StyleSpec::new(Some(Color::Rgb {
                r: 166,
                g: 209,
                b: 138,
            }))
            .bold(),
            prompt_style: StyleSpec::new(None).bold(),
            style_sequences: StyleSequences::new(),
        }
    }
}

#[derive(Clone, Copy)]
struct StyleSpec {
    fg: Option<Color>,
    bold: bool,
}

impl StyleSpec {
    fn new(fg: Option<Color>) -> Self {
        Self { fg, bold: false }
    }

    fn bold(mut self) -> Self {
        self.bold = true;
        self
    }

    fn apply(self, text: &str) -> String {
        let mut styled = style::style(text);
        if let Some(fg) = self.fg {
            styled = styled.with(fg);
        }
        if self.bold {
            styled = styled.attribute(Attribute::Bold);
        }
        format!("{styled}")
    }
}

#[derive(Clone)]
struct StyleSequences {
    reset: String,
    dim: String,
}

impl StyleSequences {
    fn new() -> Self {
        Self {
            reset: format!("{}", SetAttribute(Attribute::Reset)),
            dim: format!("{}", SetAttribute(Attribute::Dim)),
        }
    }
}

#[derive(Clone, Debug)]
struct SearchMatch {
    text: String,
    line: usize,
    col: usize,
    label: Option<char>,
    match_start: usize,
    match_end: usize,
}

#[derive(Debug)]
struct SearchInterface {
    lines: Vec<String>,
    matches: Vec<SearchMatch>,
}

impl SearchInterface {
    fn new(pane_content: &str) -> Self {
        let lines = pane_content.split('\n').map(ToString::to_string).collect();
        Self {
            lines,
            matches: Vec::new(),
        }
    }

    fn search(&mut self, query: &str) -> Vec<SearchMatch> {
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

    fn get_match_by_label(&self, label: char) -> Option<&SearchMatch> {
        self.matches.iter().find(|m| m.label == Some(label))
    }

    fn get_matches_at_line(&self, line_num: usize) -> Vec<&SearchMatch> {
        self.matches.iter().filter(|m| m.line == line_num).collect()
    }
}

#[derive(Clone, Debug)]
struct PaneDimensions {
    left: i32,
    top: i32,
    bottom: i32,
    width: i32,
    height: i32,
}

struct InteractiveUI {
    pane_id: String,
    config: Config,
    search: SearchInterface,
    search_query: String,
    current_matches: Vec<SearchMatch>,
}

impl InteractiveUI {
    fn new(pane_id: String, pane_content: &str, config: Config) -> Self {
        let search = SearchInterface::new(pane_content);

        Self {
            pane_id,
            config,
            search,
            search_query: String::new(),
            current_matches: Vec::new(),
        }
    }

    fn run(&mut self) -> Result<()> {
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
                            self.save_result("", false)?;
                            return Ok(());
                        }
                        KeyCode::Esc => {
                            self.save_result("", false)?;
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
                        KeyCode::Enter => {
                            if let Some(first) = self.current_matches.first() {
                                let text = trim_wrapping_token(
                                    &first.text,
                                    first.match_start,
                                    first.match_end,
                                );
                                self.save_result(text, true)?;
                                return Ok(());
                            }
                        }
                        KeyCode::Char(' ') if !ctrl => {
                            if let Some(first) = self.current_matches.first() {
                                let text = trim_wrapping_token(
                                    &first.text,
                                    first.match_start,
                                    first.match_end,
                                );
                                self.save_result(text, true)?;
                                return Ok(());
                            }
                        }
                        KeyCode::Char(c) if !ctrl => {
                            let label_lookup = c.to_ascii_lowercase();
                            if !self.search_query.is_empty()
                                && let Some(match_item) =
                                    self.search.get_match_by_label(label_lookup)
                            {
                                let should_paste = c.is_ascii_lowercase();
                                let text = trim_wrapping_token(
                                    &match_item.text,
                                    match_item.match_start,
                                    match_item.match_end,
                                );
                                self.save_result(text, should_paste)?;
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
        self.current_matches = self.search.search(&self.search_query);
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
                .current_matches
                .first()
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

    fn save_result(&self, text: &str, should_paste: bool) -> Result<()> {
        let pane_id = &self.pane_id;
        let buffer = format!("__flash_copy_result_{pane_id}__");
        tmux_run_quiet(&["set-buffer", "-b", &buffer, "--", text]);
        std::process::exit(if should_paste { EXIT_CODE_PASTE } else { 0 });
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

fn delete_prev_word(input: &str) -> String {
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

fn ascii_case_insensitive_eq(left: &[u8], right: &[u8]) -> bool {
    if left.len() != right.len() {
        return false;
    }
    left.iter()
        .zip(right)
        .all(|(a, b)| a.eq_ignore_ascii_case(b))
}

fn trim_wrapping_token(token: &str, match_start: usize, match_end: usize) -> &str {
    const WRAPPING_PAIRS: [(&str, &str); 6] = [
        ("(", ")"),
        ("[", "]"),
        ("{", "}"),
        ("\"", "\""),
        ("'", "'"),
        ("`", "`"),
    ];

    for (open, close) in WRAPPING_PAIRS {
        if token.starts_with(open) && token.ends_with(close) {
            let start = open.len();
            let end = token.len().saturating_sub(close.len());
            if start < end && match_start >= start && match_end <= end {
                return &token[start..end];
            }
        }
    }

    token
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

fn get_tmux_pane_id() -> Result<String> {
    tmux_output_trim(&["display-message", "-p", "#{pane_id}"], TrimMode::Trim)
        .context("failed to get pane id")
}

fn capture_pane(pane_id: &str) -> Result<String> {
    tmux_output_trim(&["capture-pane", "-p", "-J", "-t", pane_id], TrimMode::None)
        .context("failed to capture pane")
}

fn get_pane_dimensions(pane_id: &str) -> Option<PaneDimensions> {
    let out = tmux_output_trim(
        &[
            "display-message",
            "-t",
            pane_id,
            "-p",
            "#{pane_left} #{pane_top} #{pane_right} #{pane_bottom} #{pane_width} #{pane_height}",
        ],
        TrimMode::Trim,
    )
    .ok()?;

    let parts: Vec<i32> = out
        .split_whitespace()
        .filter_map(|p| p.parse::<i32>().ok())
        .collect();
    if parts.len() != 6 {
        return None;
    }

    Some(PaneDimensions {
        left: parts[0],
        top: parts[1],
        bottom: parts[3],
        width: parts[4],
        height: parts[5],
    })
}

fn calculate_popup_position(dimensions: &PaneDimensions) -> (i32, i32, i32, i32) {
    let y = if dimensions.top == 0 {
        dimensions.top
    } else {
        dimensions.bottom + 1
    };
    (dimensions.left, y, dimensions.width, dimensions.height)
}

fn tmux_output_trim(args: &[&str], trim: TrimMode) -> Result<String> {
    let output = Command::new("tmux").args(args).output()?;
    if !output.status.success() {
        bail!("tmux command failed");
    }
    let mut out = String::from_utf8_lossy(&output.stdout).to_string();
    match trim {
        TrimMode::Trim => {
            out = out.trim().to_string();
        }
        TrimMode::TrimNewlines => {
            out = out.trim_end_matches(['\n', '\r']).to_string();
        }
        TrimMode::None => {}
    }
    Ok(out)
}

fn tmux_run_quiet(args: &[&str]) -> bool {
    Command::new("tmux")
        .args(args)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

#[derive(Clone, Copy)]
enum TrimMode {
    Trim,
    TrimNewlines,
    None,
}

struct Clipboard;

impl Clipboard {
    fn copy(text: &str) -> bool {
        if std::env::var_os("TMUX").is_none() {
            return false;
        }
        if tmux_run_quiet(&["set-buffer", "-w", "--", text]) {
            return true;
        }

        if cfg!(target_os = "macos") {
            return run_with_input("pbcopy", &[], text);
        }

        if cfg!(target_os = "linux") {
            if run_with_input("xclip", &["-selection", "clipboard"], text) {
                return true;
            }
            if run_with_input("xsel", &["--clipboard", "--input"], text) {
                return true;
            }
        }

        tmux_run_quiet(&["set-buffer", "--", text])
    }

    fn copy_and_paste(text: &str, pane_id: &str, auto_paste: bool) {
        if !Self::copy(text) {
            return;
        }

        if auto_paste {
            let _ = tmux_run_quiet(&["set-buffer", "-b", "flash-paste", "--", text]);
            let _ = tmux_run_quiet(&["paste-buffer", "-b", "flash-paste", "-t", pane_id]);
        }
    }
}

fn run_with_input(cmd: &str, args: &[&str], input: &str) -> bool {
    Command::new(cmd)
        .args(args)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()
        .and_then(|mut child| {
            if let Some(stdin) = child.stdin.as_mut() {
                stdin.write_all(input.as_bytes())?;
            }
            child.wait()
        })
        .map(|status| status.success())
        .unwrap_or(false)
}

fn run_parent() -> Result<()> {
    let pane_id = get_tmux_pane_id()?;
    let pane_content = capture_pane(&pane_id).unwrap_or_default();
    let pane_buffer = format!("__flash_copy_pane_content_{pane_id}__");
    let _ = tmux_run_quiet(&["set-buffer", "-b", &pane_buffer, "--", &pane_content]);

    let (x, y, w, h) = if let Some(dimensions) = get_pane_dimensions(&pane_id) {
        calculate_popup_position(&dimensions)
    } else {
        let fallback = tmux_output_trim(
            &["display-message", "-p", "#{window_width},#{window_height}"],
            TrimMode::Trim,
        )
        .unwrap_or_else(|_| "160,40".to_string());
        let mut parts = fallback.split(',');
        let w = parts.next().and_then(|v| v.parse().ok()).unwrap_or(160);
        let h = parts.next().and_then(|v| v.parse().ok()).unwrap_or(40);
        (0, 0, w, h)
    };

    let exe = std::env::current_exe().context("failed to locate executable")?;
    let exe = exe.to_string_lossy().to_string();

    let args = vec![
        "display-popup".to_string(),
        "-E".to_string(),
        "-B".to_string(),
        "-x".to_string(),
        x.to_string(),
        "-y".to_string(),
        y.to_string(),
        "-w".to_string(),
        w.to_string(),
        "-h".to_string(),
        h.to_string(),
        exe,
        "--interactive".to_string(),
        "--pane-id".to_string(),
        pane_id.clone(),
    ];

    let status = Command::new("tmux").args(&args).status()?;

    let result_buffer = format!("__flash_copy_result_{pane_id}__");
    let result_text = tmux_output_trim(
        &["show-buffer", "-b", &result_buffer],
        TrimMode::TrimNewlines,
    )
    .ok()
    .filter(|s| !s.is_empty());

    let should_paste = status.code() == Some(EXIT_CODE_PASTE);
    if let Some(text) = result_text {
        Clipboard::copy_and_paste(&text, &pane_id, should_paste);
    }

    let _ = tmux_run_quiet(&["delete-buffer", "-b", &result_buffer]);
    let _ = tmux_run_quiet(&["delete-buffer", "-b", &pane_buffer]);

    Ok(())
}

fn run_interactive(cli: &Cli) -> Result<()> {
    let pane_id = cli
        .pane_id
        .clone()
        .context("pane-id is required in interactive mode")?;
    let config = Config::defaults();

    let pane_buffer = format!("__flash_copy_pane_content_{pane_id}__");
    let pane_content = tmux_output_trim(&["show-buffer", "-b", &pane_buffer], TrimMode::None)
        .ok()
        .unwrap_or_else(|| capture_pane(&pane_id).unwrap_or_default());

    let mut ui = InteractiveUI::new(pane_id, &pane_content, config);
    ui.run()?;

    Ok(())
}

fn main() -> Result<()> {
    if std::env::var_os("TMUX").is_none() {
        bail!("flash_tmux must be run inside tmux");
    }

    let cli = Cli::parse();
    if cli.interactive {
        run_interactive(&cli)
    } else {
        run_parent()
    }
}
