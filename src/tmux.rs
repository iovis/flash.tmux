use anyhow::{Context, Result, bail};
use std::io::Write;
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
            Some(EXIT_CODE_PASTE) => ExitAction::Paste,
            Some(EXIT_CODE_PASTE_AND_ENTER) => ExitAction::PasteAndEnter,
            Some(EXIT_CODE_PASTE_AND_SPACE) => ExitAction::PasteAndSpace,
            _ => ExitAction::CopyOnly,
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
pub enum TrimMode {
    Trim,
    TrimNewlines,
    None,
}

pub fn get_tmux_pane_id() -> Result<String> {
    tmux_output_trim(&["display-message", "-p", "#{pane_id}"], TrimMode::Trim)
        .context("failed to get pane id")
}

pub fn capture_pane(pane_id: &str) -> Result<String> {
    tmux_output_trim(&["capture-pane", "-p", "-J", "-t", pane_id], TrimMode::None)
        .context("failed to capture pane")
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

pub fn tmux_output_trim(args: &[&str], trim: TrimMode) -> Result<String> {
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

pub fn tmux_run_quiet(args: &[&str]) -> bool {
    Command::new("tmux")
        .args(args)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

pub struct Clipboard;

impl Clipboard {
    pub fn copy(text: &str) -> bool {
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
            let _ = tmux_run_quiet(&["set-buffer", "-b", "flash-paste", "--", text]);
            let _ = tmux_run_quiet(&["paste-buffer", "-b", "flash-paste", "-t", pane_id]);
            if let Some(key) = forward_key {
                let key_name = match key {
                    ForwardKey::Enter => "Enter",
                    ForwardKey::Space => "Space",
                };
                let _ = tmux_run_quiet(&["send-keys", "-t", pane_id, key_name]);
            }
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
