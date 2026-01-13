#!/usr/bin/env bash
# flash.tmux plugin entrypoint for TPM

PLUGIN_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

get_tmux_option() {
  local option="$1"
  local default_value="$2"
  local option_override
  option_override="$(tmux show-option -gqv "$option")"
  if [ -z "$option_override" ]; then
    echo "$default_value"
  else
    echo "$option_override"
  fi
}

bind_key=$(get_tmux_option "@flash-copy-bind-key" "F")

tmux bind-key "$bind_key" run-shell "$PLUGIN_DIR/bin/flash-tmux"
