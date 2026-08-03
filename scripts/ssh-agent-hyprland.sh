#!/usr/bin/env bash
set -euo pipefail

SSH_AGENT_SOCKET="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/ssh-agent.socket"

check_ssh_agent() {
    if [[ -S "$SSH_AGENT_SOCKET" ]]; then
        SSH_AUTH_SOCK="$SSH_AGENT_SOCKET" ssh-add -l &>/dev/null
        return $?
    fi
    return 1
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

case "${1:-check}" in
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
        echo "Usage: $(basename "$0") {check|add-keys}"
        echo ""
        echo "ssh-agent is managed as a systemd user service (ssh-agent.service)."
        echo "It starts automatically on login and persists across Hyprland restarts."
        exit 1
        ;;
esac
