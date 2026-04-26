use anyhow::{Context, Result, bail};
use clap::Parser;
use std::process::Command;

use flash_tmux::config::{Config, LabelActionMode};
use flash_tmux::tmux;
use flash_tmux::ui::{InteractiveUI, SelectedText};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    #[arg(long)]
    interactive: bool,
    #[arg(long)]
    pane_id: Option<String>,
    #[arg(long, hide = true, allow_hyphen_values = true)]
    copy_mode_range: Option<String>,
    /// Reverse label behavior: lowercase=copy only, uppercase=copy+paste
    #[arg(short = 'r', long)]
    reverse_label: bool,
}

fn run_parent(cli: &Cli) -> Result<()> {
    let pane_info = tmux::get_current_pane_info()?;
    let pane_id = &pane_info.pane_id;
    let (x, y, w, h) = tmux::calculate_popup_position(&pane_info.dimensions);

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

    let args = if let Some(range) = &pane_info.copy_mode_line_range {
        let mut mut_args = args;
        mut_args.push(format!("--copy-mode-range={}:{}", range.start, range.end));
        mut_args
    } else {
        args
    };

    let args = if cli.reverse_label {
        let mut mut_args = args;
        mut_args.push("-r".to_string());
        mut_args
    } else {
        args
    };

    let _ = Command::new("tmux").args(&args).status()?;

    Ok(())
}

fn run_interactive(cli: &Cli) -> Result<tmux::ExitAction> {
    let pane_id = cli
        .pane_id
        .clone()
        .context("pane-id is required in interactive mode")?;
    let copy_mode_line_range = cli
        .copy_mode_range
        .as_deref()
        .map(parse_copy_mode_range)
        .transpose()?;
    let in_copy_mode = copy_mode_line_range.is_some();
    let mut config = Config::defaults();
    config.label_action_mode = cli.label_action_mode();

    let pane_content = tmux::capture_pane_with_range(&pane_id, copy_mode_line_range.as_ref())?;

    let mut ui = InteractiveUI::new(&pane_content, config);
    let selection = ui.run()?;
    apply_selection(&pane_id, in_copy_mode, &selection);

    Ok(selection.action)
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    if std::env::var_os("TMUX").is_none() {
        bail!("flash_tmux must be run inside tmux");
    }

    if cli.interactive {
        let action = run_interactive(&cli)?;
        std::process::exit(action.exit_code());
    } else {
        run_parent(&cli)
    }
}

fn parse_copy_mode_range(value: &str) -> Result<tmux::CopyModeLineRange> {
    let (start, end) = value
        .split_once(':')
        .context("copy-mode range must be encoded as <start>:<end>")?;
    if start.is_empty() || end.is_empty() {
        bail!("copy-mode range must include start and end");
    }

    Ok(tmux::CopyModeLineRange {
        start: start.to_string(),
        end: end.to_string(),
    })
}

fn apply_selection(pane_id: &str, in_copy_mode: bool, selection: &SelectedText) {
    if selection.text.is_empty() || matches!(selection.action, tmux::ExitAction::Cancel) {
        return;
    }

    if in_copy_mode && selection.action.should_paste() {
        tmux::exit_copy_mode(pane_id);
    }

    tmux::Clipboard::copy_and_paste(
        &selection.text,
        pane_id,
        selection.action.should_paste(),
        selection.action.forward_key(),
    );
}

impl Cli {
    fn label_action_mode(&self) -> LabelActionMode {
        if self.reverse_label {
            LabelActionMode::Reversed
        } else {
            LabelActionMode::Default
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_copy_mode_range_accepts_negative_start() {
        let range = parse_copy_mode_range("-10:13").expect("range should parse");

        assert_eq!(range.start, "-10");
        assert_eq!(range.end, "13");
    }

    #[test]
    fn parse_copy_mode_range_rejects_missing_bound() {
        assert!(parse_copy_mode_range("-10:").is_err());
        assert!(parse_copy_mode_range(":13").is_err());
        assert!(parse_copy_mode_range("-10").is_err());
    }
}
