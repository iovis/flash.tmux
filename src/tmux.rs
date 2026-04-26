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

#[derive(Clone, Debug)]
pub struct CopyModeLineRange {
    pub start: String,
    pub end: String,
}

#[derive(Clone, Debug)]
pub struct PaneInfo {
    pub pane_id: String,
    pub copy_mode_line_range: Option<CopyModeLineRange>,
    pub dimensions: PaneDimensions,
}

#[derive(Clone, Copy)]
enum TrimMode {
    TrimNewlines,
    None,
}

pub fn get_current_pane_info() -> Result<PaneInfo> {
    let out = tmux_output_trim(
        &[
            "display-message",
            "-p",
            "#{pane_id}\t#{pane_mode}\t#{scroll_position}\t#{pane_height}\t#{pane_left}\t#{pane_top}\t#{pane_right}\t#{pane_bottom}\t#{pane_width}",
        ],
        TrimMode::TrimNewlines,
    )
    .context("failed to get current pane info")?;

    parse_current_pane_info(&out).context("failed to parse current pane info")
}

fn parse_current_pane_info(out: &str) -> Option<PaneInfo> {
    let mut fields = out.split('\t');
    let pane_id = fields.next()?.to_string();
    let pane_mode = fields.next()?;
    let scroll = parse_i32_or_default(fields.next()?, 0)?;
    let height: i32 = fields.next()?.parse().ok()?;
    let left: i32 = fields.next()?.parse().ok()?;
    let top: i32 = fields.next()?.parse().ok()?;
    fields.next()?.parse::<i32>().ok()?;
    let bottom: i32 = fields.next()?.parse().ok()?;
    let width: i32 = fields.next()?.parse().ok()?;

    if fields.next().is_some() {
        return None;
    }

    let copy_mode_line_range = (pane_mode == "copy-mode").then(|| CopyModeLineRange {
        start: (-scroll).to_string(),
        end: (-scroll + height - 1).to_string(),
    });

    Some(PaneInfo {
        pane_id,
        copy_mode_line_range,
        dimensions: PaneDimensions {
            left,
            top,
            bottom,
            width,
            height,
        },
    })
}

fn parse_i32_or_default(value: &str, default: i32) -> Option<i32> {
    if value.is_empty() {
        Some(default)
    } else {
        value.parse().ok()
    }
}

pub fn capture_pane_with_range(
    pane_id: &str,
    copy_mode_line_range: Option<&CopyModeLineRange>,
) -> Result<String> {
    if let Some(range) = copy_mode_line_range {
        return tmux_output_trim(
            &[
                "capture-pane",
                "-p",
                "-J",
                "-S",
                &range.start,
                "-E",
                &range.end,
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

pub fn calculate_popup_position(dimensions: &PaneDimensions) -> (i32, i32, i32, i32) {
    let y = if dimensions.top == 0 {
        dimensions.top
    } else {
        dimensions.bottom + 1
    };
    (dimensions.left, y, dimensions.width, dimensions.height)
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

fn write_buffer(buffer_name: &str, text: &str) -> bool {
    tmux_run_quiet(&["set-buffer", "-b", buffer_name, "--", text])
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
        // The child applies copy-only before exiting, so the exit code is informational.
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

    #[test]
    fn parse_current_pane_info_handles_normal_mode() {
        let info = parse_current_pane_info("%1\t\t\t24\t0\t0\t79\t23\t80")
            .expect("pane info should parse");

        assert_eq!(info.pane_id, "%1");
        assert!(info.copy_mode_line_range.is_none());
        assert_eq!(info.dimensions.left, 0);
        assert_eq!(info.dimensions.top, 0);
        assert_eq!(info.dimensions.bottom, 23);
        assert_eq!(info.dimensions.width, 80);
        assert_eq!(info.dimensions.height, 24);
    }

    #[test]
    fn parse_current_pane_info_handles_copy_mode_range() {
        let info = parse_current_pane_info("%2\tcopy-mode\t10\t24\t1\t2\t80\t25\t80")
            .expect("pane info should parse");
        let range = info.copy_mode_line_range.expect("copy-mode range");

        assert_eq!(info.pane_id, "%2");
        assert_eq!(range.start, "-10");
        assert_eq!(range.end, "13");
        assert_eq!(info.dimensions.left, 1);
        assert_eq!(info.dimensions.top, 2);
        assert_eq!(info.dimensions.bottom, 25);
        assert_eq!(info.dimensions.width, 80);
        assert_eq!(info.dimensions.height, 24);
    }

    #[test]
    fn parse_current_pane_info_rejects_malformed_output() {
        assert!(parse_current_pane_info("%1\tcopy-mode").is_none());
        assert!(parse_current_pane_info("%1\t\t\t24\t0\t0\t79\t23\t80\textra").is_none());
    }
}
