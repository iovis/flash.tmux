use anyhow::{Context, Result, bail};
use clap::Parser;
use std::process::Command;

use flash_tmux::config::Config;
use flash_tmux::tmux::{
    Clipboard, ExitAction, TrimMode, calculate_popup_position, capture_pane, get_pane_dimensions,
    get_tmux_pane_id, tmux_output_trim, tmux_run_quiet,
};
use flash_tmux::ui::InteractiveUI;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    #[arg(long)]
    interactive: bool,
    #[arg(long)]
    pane_id: Option<String>,
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

    let action = ExitAction::from_exit_code(status.code());
    if let Some(text) = result_text {
        Clipboard::copy_and_paste(&text, &pane_id, action.should_paste(), action.forward_key());
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
