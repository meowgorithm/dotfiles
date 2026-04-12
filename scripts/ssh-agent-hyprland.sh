#!/usr/bin/env bash
set -euo pipefail

SSH_AGENT_SOCKET="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/ssh-agent.socket"
HYPRLAND_CONF="${XDG_CONFIG_HOME:-$HOME/.config}/hypr/hyprland.conf"
PROFILE_D="/etc/profile.d/ssh-agent-hyprland.sh"

check_ssh_agent() {
    if [[ -S "$SSH_AGENT_SOCKET" ]]; then
        SSH_AUTH_SOCK="$SSH_AGENT_SOCKET" ssh-add -l &>/dev/null
        return $?
    fi
    return 1
}

start_agent() {
    mkdir -p "$(dirname "$SSH_AGENT_SOCKET")"
    ssh-agent -D -a "$SSH_AGENT_SOCKET" &
    echo "ssh-agent started on $SSH_AGENT_SOCKET"
}

add_keys() {
    local keys=()
    for key in "$HOME/.ssh"/id_*; do
        [[ -f "$key" && "$key" != *.pub && "$key" != *.so ]] && keys+=("$key")
    done
    if (( ${#keys[@]} > 0 )); then
        SSH_AUTH_SOCK="$SSH_AGENT_SOCKET" ssh-add "${keys[@]}" 2>/dev/null
        echo "Added ${#keys[@]} key(s) to ssh-agent"
    else
        echo "No SSH keys found in $HOME/.ssh"
    fi
}

setup_hyprland() {
    if ! grep -q 'exec-once.*ssh-agent' "$HYPRLAND_CONF" 2>/dev/null; then
        echo "exec-once = ssh-agent -D -a \$XDG_RUNTIME_DIR/ssh-agent.socket" \
            >> "$HYPRLAND_CONF"
        echo "Added ssh-agent exec-once to $HYPRLAND_CONF"
    else
        echo "ssh-agent exec-once already in $HYPRLAND_CONF"
    fi
}

setup_env_export() {
    if [[ ! -f "$PROFILE_D" ]] && [[ ! -f "${XDG_CONFIG_HOME:-$HOME/.config}/environment.d/ssh-agent.conf" ]]; then
        local env_dir="${XDG_CONFIG_HOME:-$HOME/.config}/environment.d"
        mkdir -p "$env_dir"
        echo "SSH_AUTH_SOCK=$SSH_AGENT_SOCKET" > "$env_dir/ssh-agent.conf"
        echo "Wrote SSH_AUTH_SOCK to $env_dir/ssh-agent.conf"
    else
        echo "SSH_AUTH_SOCK export already configured"
    fi
}

case "${1:-setup}" in
    setup)
        setup_hyprland
        setup_env_export
        echo ""
        echo "Setup complete. On next login, ssh-agent will start automatically."
        echo "Run '$(basename "$0") check' to verify, or '$(basename "$0") add-keys' to add keys now."
        ;;
    check)
        if check_ssh_agent; then
            echo "ssh-agent is running and keys are loaded:"
            SSH_AUTH_SOCK="$SSH_AGENT_SOCKET" ssh-add -l
        elif [[ -S "$SSH_AGENT_SOCKET" ]]; then
            echo "ssh-agent socket exists but no keys loaded"
        else
            echo "ssh-agent is not running"
        fi
        ;;
    add-keys)
        add_keys
        ;;
    *)
        echo "Usage: $(basename "$0") {setup|check|add-keys}"
        exit 1
        ;;
esac
