use anyhow::{Context, Result, bail, ensure};
use clap::Parser;
use std::process::Command;

use flash_tmux::config::Config;
use flash_tmux::tmux;
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
    let pane_id = tmux::get_tmux_pane_id()?;
    let in_copy_mode = tmux::is_in_copy_mode(&pane_id);
    let pane_content = tmux::capture_pane(&pane_id, in_copy_mode)?;
    ensure!(
        tmux::write_pane_content_buffer(&pane_id, &pane_content),
        "failed to write pane content to tmux buffer"
    );
    ensure!(
        tmux::write_result_buffer(&pane_id, ""),
        "failed to clear tmux result buffer"
    );

    let dimensions =
        tmux::get_pane_dimensions(&pane_id).context("failed to get pane dimensions")?;
    let (x, y, w, h) = tmux::calculate_popup_position(&dimensions);

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

    let result_text = tmux::read_result_buffer(&pane_id)
        .ok()
        .filter(|s| !s.is_empty());

    let action = tmux::ExitAction::from_exit_code(status.code());
    if let Some(text) = result_text
        && !matches!(action, tmux::ExitAction::Cancel)
    {
        if in_copy_mode && action.should_paste() {
            tmux::exit_copy_mode(&pane_id);
        }
        tmux::Clipboard::copy_and_paste(
            &text,
            &pane_id,
            action.should_paste(),
            action.forward_key(),
        );
    }

    let _ = tmux::delete_buffers(&pane_id);

    Ok(())
}

fn run_interactive(cli: &Cli) -> Result<tmux::ExitAction> {
    let pane_id = cli
        .pane_id
        .clone()
        .context("pane-id is required in interactive mode")?;
    let config = Config::defaults();

    let pane_content = tmux::read_pane_content_buffer(&pane_id)
        .ok()
        .unwrap_or_else(|| tmux::capture_pane(&pane_id, false).unwrap_or_default());

    let mut ui = InteractiveUI::new(pane_id, &pane_content, config);
    ui.run()
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
        run_parent()
    }
}
