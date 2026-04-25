use anyhow::{Context, Result, bail};
use std::process::Command;

const EXIT_CODE_PASTE: i32 = 10;
const EXIT_CODE_PASTE_AND_ENTER: i32 = 11;
const EXIT_CODE_PASTE_AND_SPACE: i32 = 12;

#[derive(Copy, Clone)]
pub enum ExitAction {
    Cancel,
    CopyOnly,
    Paste,
    PasteAndEnter,
    PasteAndSpace,
}

#[derive(Copy, Clone)]
pub enum ForwardKey {
    Enter,
    Space,
}

impl ExitAction {
    pub fn exit_code(self) -> i32 {
        match self {
            ExitAction::Cancel | ExitAction::CopyOnly => 0,
            ExitAction::Paste => EXIT_CODE_PASTE,
            ExitAction::PasteAndEnter => EXIT_CODE_PASTE_AND_ENTER,
            ExitAction::PasteAndSpace => EXIT_CODE_PASTE_AND_SPACE,
        }
    }

    pub fn should_paste(self) -> bool {
        matches!(
            self,
            ExitAction::Paste | ExitAction::PasteAndEnter | ExitAction::PasteAndSpace
        )
    }

    pub fn forward_key(self) -> Option<ForwardKey> {
        match self {
            ExitAction::PasteAndEnter => Some(ForwardKey::Enter),
            ExitAction::PasteAndSpace => Some(ForwardKey::Space),
            _ => None,
        }
    }

    pub fn from_exit_code(code: Option<i32>) -> Self {
        match code {
            Some(0) => ExitAction::CopyOnly,
            Some(EXIT_CODE_PASTE) => ExitAction::Paste,
            Some(EXIT_CODE_PASTE_AND_ENTER) => ExitAction::PasteAndEnter,
            Some(EXIT_CODE_PASTE_AND_SPACE) => ExitAction::PasteAndSpace,
            _ => ExitAction::Cancel,
        }
    }
}

#[derive(Clone, Debug)]
pub struct PaneDimensions {
    pub left: i32,
    pub top: i32,
    pub bottom: i32,
    pub width: i32,
    pub height: i32,
}

#[derive(Clone, Copy)]
enum TrimMode {
    Trim,
    TrimNewlines,
    None,
}

pub fn get_tmux_pane_id() -> Result<String> {
    tmux_output_trim(&["display-message", "-p", "#{pane_id}"], TrimMode::Trim)
        .context("failed to get pane id")
}

pub fn capture_pane(pane_id: &str, copy_mode: bool) -> Result<String> {
    if copy_mode && let Some((start, end)) = copy_mode_line_range(pane_id) {
        return tmux_output_trim(
            &[
                "capture-pane",
                "-p",
                "-J",
                "-S",
                &start,
                "-E",
                &end,
                "-t",
                pane_id,
            ],
            TrimMode::None,
        )
        .context("failed to capture pane in copy-mode");
    }

    tmux_output_trim(&["capture-pane", "-p", "-J", "-t", pane_id], TrimMode::None)
        .context("failed to capture pane")
}

fn copy_mode_line_range(pane_id: &str) -> Option<(String, String)> {
    let out = tmux_output_trim(
        &[
            "display-message",
            "-t",
            pane_id,
            "-p",
            "#{scroll_position} #{pane_height}",
        ],
        TrimMode::Trim,
    )
    .ok()?;

    let mut parts = out.split_whitespace();
    let scroll: i32 = parts.next()?.parse().ok()?;
    let height: i32 = parts.next()?.parse().ok()?;

    let start = -scroll;
    let end = -scroll + height - 1;

    Some((start.to_string(), end.to_string()))
}

pub fn get_pane_dimensions(pane_id: &str) -> Option<PaneDimensions> {
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

pub fn calculate_popup_position(dimensions: &PaneDimensions) -> (i32, i32, i32, i32) {
    let y = if dimensions.top == 0 {
        dimensions.top
    } else {
        dimensions.bottom + 1
    };
    (dimensions.left, y, dimensions.width, dimensions.height)
}

pub fn is_in_copy_mode(pane_id: &str) -> bool {
    tmux_output_trim(
        &["display-message", "-t", pane_id, "-p", "#{pane_mode}"],
        TrimMode::Trim,
    )
    .is_ok_and(|mode| mode == "copy-mode")
}

pub fn exit_copy_mode(pane_id: &str) {
    tmux_run_quiet(&["copy-mode", "-q", "-t", pane_id]);
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
        .is_ok_and(|o| o.status.success())
}

pub struct Clipboard;

impl Clipboard {
    pub fn copy(text: &str) -> bool {
        if tmux_run_quiet(&["set-buffer", "-w", "--", text]) {
            return true;
        }

        let _ = Command::new("tmux")
            .args([
                "display-message",
                "flash.tmux: failed to copy to clipboard (OSC52)",
            ])
            .status();

        false
    }

    pub fn copy_and_paste(
        text: &str,
        pane_id: &str,
        auto_paste: bool,
        forward_key: Option<ForwardKey>,
    ) {
        if !Self::copy(text) {
            return;
        }

        if auto_paste {
            let _ = write_buffer("flash-paste", text);
            let _ = paste_buffer("flash-paste", pane_id);
            if let Some(key) = forward_key {
                let _ = send_keys(pane_id, key);
            }
        }
    }
}

pub fn write_pane_content_buffer(pane_id: &str, content: &str) -> bool {
    let buffer = pane_content_buffer_name(pane_id);
    write_buffer(&buffer, content)
}

pub fn read_pane_content_buffer(pane_id: &str) -> Result<String> {
    let buffer = pane_content_buffer_name(pane_id);
    read_buffer_raw(&buffer)
}

pub fn write_result_buffer(pane_id: &str, text: &str) -> bool {
    let buffer = result_buffer_name(pane_id);
    write_buffer(&buffer, text)
}

pub fn read_result_buffer(pane_id: &str) -> Result<String> {
    let buffer = result_buffer_name(pane_id);
    read_buffer_trimmed(&buffer)
}

pub fn delete_buffers(pane_id: &str) -> bool {
    let result = delete_buffer(&result_buffer_name(pane_id));
    let pane = delete_buffer(&pane_content_buffer_name(pane_id));
    result && pane
}

fn pane_content_buffer_name(pane_id: &str) -> String {
    format!("__flash_copy_pane_content_{pane_id}__")
}

fn result_buffer_name(pane_id: &str) -> String {
    format!("__flash_copy_result_{pane_id}__")
}

pub fn read_buffer_raw(buffer_name: &str) -> Result<String> {
    tmux_output_trim(&["show-buffer", "-b", buffer_name], TrimMode::None)
}

fn read_buffer_trimmed(buffer_name: &str) -> Result<String> {
    tmux_output_trim(&["show-buffer", "-b", buffer_name], TrimMode::TrimNewlines)
}

fn write_buffer(buffer_name: &str, text: &str) -> bool {
    tmux_run_quiet(&["set-buffer", "-b", buffer_name, "--", text])
}

fn delete_buffer(buffer_name: &str) -> bool {
    tmux_run_quiet(&["delete-buffer", "-b", buffer_name])
}

fn paste_buffer(buffer_name: &str, pane_id: &str) -> bool {
    tmux_run_quiet(&["paste-buffer", "-b", buffer_name, "-t", pane_id])
}

fn send_keys(pane_id: &str, key: ForwardKey) -> bool {
    let key_name = match key {
        ForwardKey::Enter => "Enter",
        ForwardKey::Space => "Space",
    };
    tmux_run_quiet(&["send-keys", "-t", pane_id, key_name])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exit_action_round_trip_paste() {
        let code = ExitAction::Paste.exit_code();
        assert!(ExitAction::from_exit_code(Some(code)).should_paste());
        assert!(
            ExitAction::from_exit_code(Some(code))
                .forward_key()
                .is_none()
        );
    }

    #[test]
    fn exit_action_round_trip_paste_and_enter() {
        let code = ExitAction::PasteAndEnter.exit_code();
        assert!(ExitAction::from_exit_code(Some(code)).should_paste());
        assert!(matches!(
            ExitAction::from_exit_code(Some(code)).forward_key(),
            Some(ForwardKey::Enter)
        ));
    }

    #[test]
    fn exit_action_round_trip_paste_and_space() {
        let code = ExitAction::PasteAndSpace.exit_code();
        assert!(ExitAction::from_exit_code(Some(code)).should_paste());
        assert!(matches!(
            ExitAction::from_exit_code(Some(code)).forward_key(),
            Some(ForwardKey::Space)
        ));
    }

    #[test]
    fn exit_action_round_trip_copy_only() {
        let code = ExitAction::CopyOnly.exit_code();
        assert!(!ExitAction::from_exit_code(Some(code)).should_paste());
    }

    #[test]
    fn exit_action_cancel_and_copy_only_share_exit_code() {
        // Cancel and CopyOnly both use exit code 0.
        // The parent distinguishes them by whether the result buffer is empty.
        assert_eq!(
            ExitAction::Cancel.exit_code(),
            ExitAction::CopyOnly.exit_code()
        );
        assert_eq!(ExitAction::Cancel.exit_code(), 0);
    }

    #[test]
    fn exit_action_unknown_code_is_cancel() {
        assert!(!ExitAction::from_exit_code(Some(99)).should_paste());
        assert!(!ExitAction::from_exit_code(None).should_paste());
    }
}
