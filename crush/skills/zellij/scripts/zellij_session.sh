#!/usr/bin/env bash
set -euo pipefail

# Manage zellij sessions, tabs, and panes.
#
# Usage:
#   zellij_session.sh <command> [options]
#
# Sessions:
#   new <name> [workdir]               Create detached session (idempotent)
#   kill <name>                        Kill session (resurrectable)
#   delete <name>                      Kill session and purge resurrectable state
#   list                               Full tree: sessions, tabs, panes
#   list-sessions                      Session names only
#   exists <name>                      Check if session exists (exit 0/1)
#   attached <name>                    Check if session has clients (exit 0/1)
#
# Tabs:
#   new-tab <session> [name]           Create tab, prints tab id
#   list-tabs <session>                List tabs (id, position, name)
#   go-to-tab <session> <index|name>   Focus tab by index or name
#   close-tab <session> [tab_id]       Close tab (focused tab if no id)
#   rename-tab <session> <tab_id> <n>  Rename tab by stable id
#
# Panes (pane = terminal_1, plugin_2, or bare number; default: focused):
#   split <session> [right|down] [name] [workdir]  New shell pane, prints pane id
#   run <session> [name] [workdir] <cmd...>        New pane running cmd, prints pane id
#                                                  (no shell: use bash -c '...' for compound cmds)
#   list-panes <session> [--json|--all]            List panes
#   kill-pane <session> [pane]         Close pane
#   focus <session> <pane>             Focus pane
#   rename-pane <session> [pane] <n>   Rename pane
#   resize <session> [pane] <increase|decrease> <right|down|up|left>
#   clear <session> [pane]             Clear pane buffers (viewport + scrollback)
#   fullscreen <session> [pane]        Toggle fullscreen on pane
#   info <session> [pane]              Show pane details as JSON
#   -h, --help                         Show this help

usage() {
	sed -n '4,/^$/p' "$0" | sed 's/^# \?//' >&2
	exit 1
}

[[ $# -lt 1 ]] && usage

CMD="$1"
shift

session_exists() {
	zellij list-sessions -s 2>/dev/null | grep -qx "$1"
}

# Resolve a pane argument to a full pane id. With no argument, pick the
# focused selectable terminal pane, else the first selectable terminal pane.
resolve_pane() {
	local session="$1" pane="${2:-}"
	if [[ -n "$pane" ]]; then
		case "$pane" in
		terminal_* | plugin_*) printf '%s' "$pane" ;;
		*) printf 'terminal_%s' "$pane" ;;
		esac
		return
	fi
	local resolved
	resolved=$(zellij --session "$session" action list-panes --json 2>/dev/null |
		jq -r '
			[.[] | select(.is_plugin == false and .is_selectable)] as $sel
			| (([$sel[] | select(.is_focused)] | first) // ($sel | first))
			| if . == null then "" else "terminal_\(.id)" end
		') || true
	if [[ -z "$resolved" ]]; then
		echo "Error: no terminal pane found in session '$session'" >&2
		exit 1
	fi
	printf '%s' "$resolved"
}

require_session() {
	[[ -n "${1:-}" ]] || {
		echo "Error: session required" >&2
		exit 1
	}
}

case "$CMD" in
new)
	NAME="${1:-}"
	WORKDIR="${2:-$PWD}"
	[[ -z "$NAME" ]] && {
		echo "Error: session name required" >&2
		exit 1
	}
	if session_exists "$NAME"; then
		echo "$NAME"
		exit 0
	fi
	# The initial pane inherits the cwd of the creating process.
	(cd "$WORKDIR" && zellij attach --create-background --create "$NAME" >/dev/null 2>&1) || true
	for _ in 1 2 3 4 5 6 7 8 9 10; do
		session_exists "$NAME" && break
		sleep 0.3
	done
	if session_exists "$NAME"; then
		echo "$NAME"
	else
		echo "Error creating session '$NAME'" >&2
		exit 1
	fi
	;;

kill)
	SESSION="${1:-}"
	require_session "$SESSION"
	zellij kill-session "$SESSION" 2>/dev/null && echo "killed $SESSION" || echo "no such session: $SESSION" >&2
	;;

delete)
	SESSION="${1:-}"
	require_session "$SESSION"
	zellij delete-session "$SESSION" 2>/dev/null && echo "deleted $SESSION" || echo "no such session: $SESSION" >&2
	;;

list)
	zellij list-sessions -s 2>/dev/null | sort | while read -r sess; do
		echo "=== session: $sess ==="
		zellij --session "$sess" action list-tabs 2>/dev/null | tail -n +2 | while read -r tid tpos tname; do
			echo "  tab: $tpos '$tname' (id=$tid)"
		done
		zellij --session "$sess" action list-panes --json 2>/dev/null | jq -r '
			.[]
			| "    pane: \(if .is_plugin then "plugin_" else "terminal_" end)\(.id) [\(.title)] cmd=\(.pane_command // .plugin_url // "-") focused=\(.is_focused) exited=\(.exited) tab=[\(.tab_name)]"
		'
	done
	;;

list-sessions)
	zellij list-sessions -s 2>/dev/null || echo "(no sessions)"
	;;

exists)
	SESSION="${1:-}"
	require_session "$SESSION"
	session_exists "$SESSION"
	;;

attached)
	SESSION="${1:-}"
	require_session "$SESSION"
	COUNT=$(zellij --session "$SESSION" action list-clients 2>/dev/null | tail -n +2 | wc -l | tr -d ' ')
	[[ "${COUNT:-0}" -gt 0 ]]
	;;

new-tab)
	SESSION="${1:-}"
	NAME="${2:-}"
	require_session "$SESSION"
	ARGS=(action new-tab)
	[[ -n "$NAME" ]] && ARGS+=(--name "$NAME")
	zellij --session "$SESSION" "${ARGS[@]}"
	;;

list-tabs)
	SESSION="${1:-}"
	require_session "$SESSION"
	zellij --session "$SESSION" action list-tabs
	;;

go-to-tab)
	SESSION="${1:-}"
	TAB="${2:-}"
	require_session "$SESSION"
	[[ -z "$TAB" ]] && {
		echo "Error: tab index or name required" >&2
		exit 1
	}
	if [[ "$TAB" =~ ^[0-9]+$ ]]; then
		zellij --session "$SESSION" action go-to-tab "$TAB"
	else
		zellij --session "$SESSION" action go-to-tab-name "$TAB"
	fi
	;;

close-tab)
	SESSION="${1:-}"
	TAB_ID="${2:-}"
	require_session "$SESSION"
	if [[ -n "$TAB_ID" ]]; then
		zellij --session "$SESSION" action close-tab-by-id "$TAB_ID"
	else
		zellij --session "$SESSION" action close-tab
	fi
	;;

rename-tab)
	SESSION="${1:-}"
	TAB_ID="${2:-}"
	NAME="${3:-}"
	require_session "$SESSION"
	[[ -z "$TAB_ID" || -z "$NAME" ]] && {
		echo "Error: tab id and name required" >&2
		exit 1
	}
	zellij --session "$SESSION" action rename-tab-by-id "$TAB_ID" "$NAME"
	echo "renamed tab $TAB_ID -> $NAME"
	;;

split)
	SESSION="${1:-}"
	DIRECTION="${2:-down}"
	NAME="${3:-}"
	WORKDIR="${4:-}"
	require_session "$SESSION"
	case "$DIRECTION" in
	right | down) ;;
	*)
		echo "Error: direction must be 'right' or 'down'" >&2
		exit 1
		;;
	esac
	ARGS=(action new-pane --direction "$DIRECTION")
	[[ -n "$NAME" ]] && ARGS+=(--name "$NAME")
	PANE_ID=$(zellij --session "$SESSION" "${ARGS[@]}") || {
		echo "Error splitting pane: $PANE_ID" >&2
		exit 1
	}
	# new-pane --cwd is unreliable (zellij 0.45); cd explicitly instead.
	if [[ -n "$WORKDIR" ]]; then
		sleep 0.3
		zellij --session "$SESSION" action write-chars --pane-id "$PANE_ID" "cd $(printf '%q' "$WORKDIR")"
		zellij --session "$SESSION" action send-keys --pane-id "$PANE_ID" Enter
	fi
	echo "$PANE_ID"
	;;

run)
	SESSION="${1:-}"
	NAME="${2:-}"
	WORKDIR="${3:-}"
	require_session "$SESSION"
	[[ $# -lt 4 ]] && {
		echo "Error: run requires <session> [name] [workdir] <cmd...>" >&2
		echo "Use \"\" to skip name or workdir." >&2
		exit 1
	}
	shift 3
	ARGS=(run)
	[[ -n "$NAME" ]] && ARGS+=(--name "$NAME")
	[[ -n "$WORKDIR" ]] && ARGS+=(--cwd "$WORKDIR")
	PANE_ID=$(zellij --session "$SESSION" "${ARGS[@]}" -- "$@") || {
		echo "Error running command: $PANE_ID" >&2
		exit 1
	}
	echo "$PANE_ID"
	;;

list-panes)
	SESSION="${1:-}"
	FORMAT="${2:-}"
	require_session "$SESSION"
	case "$FORMAT" in
	--json | -j) zellij --session "$SESSION" action list-panes --json ;;
	--all | -a) zellij --session "$SESSION" action list-panes --all ;;
	*) zellij --session "$SESSION" action list-panes ;;
	esac
	;;

kill-pane)
	SESSION="${1:-}"
	require_session "$SESSION"
	PANE_ID=$(resolve_pane "$SESSION" "${2:-}")
	zellij --session "$SESSION" action close-pane --pane-id "$PANE_ID" && echo "closed $PANE_ID"
	;;

focus)
	SESSION="${1:-}"
	PANE="${2:-}"
	require_session "$SESSION"
	[[ -z "$PANE" ]] && {
		echo "Error: pane required" >&2
		exit 1
	}
	PANE_ID=$(resolve_pane "$SESSION" "$PANE")
	zellij --session "$SESSION" action focus-pane-id "$PANE_ID"
	echo "focused $PANE_ID"
	;;

rename-pane)
	SESSION="${1:-}"
	PANE="${2:-}"
	NAME="${3:-}"
	require_session "$SESSION"
	[[ -z "$NAME" ]] && {
		echo "Error: pane and name required" >&2
		exit 1
	}
	PANE_ID=$(resolve_pane "$SESSION" "$PANE")
	zellij --session "$SESSION" action rename-pane --pane-id "$PANE_ID" "$NAME"
	echo "renamed $PANE_ID -> $NAME"
	;;

resize)
	SESSION="${1:-}"
	PANE="${2:-}"
	RESIZE="${3:-}"
	DIRECTION="${4:-}"
	require_session "$SESSION"
	[[ -z "$RESIZE" || -z "$DIRECTION" ]] && {
		echo "Error: resize requires <increase|decrease> <right|down|up|left>" >&2
		exit 1
	}
	PANE_ID=$(resolve_pane "$SESSION" "$PANE")
	zellij --session "$SESSION" action resize --pane-id "$PANE_ID" "$RESIZE" "$DIRECTION"
	echo "resized $PANE_ID $RESIZE $DIRECTION"
	;;

clear)
	SESSION="${1:-}"
	require_session "$SESSION"
	PANE_ID=$(resolve_pane "$SESSION" "${2:-}")
	zellij --session "$SESSION" action clear --pane-id "$PANE_ID"
	echo "cleared $PANE_ID"
	;;

fullscreen)
	SESSION="${1:-}"
	require_session "$SESSION"
	PANE_ID=$(resolve_pane "$SESSION" "${2:-}")
	zellij --session "$SESSION" action toggle-fullscreen --pane-id "$PANE_ID"
	echo "fullscreen toggled on $PANE_ID"
	;;

info)
	SESSION="${1:-}"
	require_session "$SESSION"
	PANE_ID=$(resolve_pane "$SESSION" "${2:-}")
	TYPE="${PANE_ID%%_*}"
	NUM="${PANE_ID##*_}"
	IS_PLUGIN=false
	[[ "$TYPE" == "plugin" ]] && IS_PLUGIN=true
	zellij --session "$SESSION" action list-panes --json |
		jq --argjson id "$NUM" --argjson plugin "$IS_PLUGIN" \
			'.[] | select(.id == $id and .is_plugin == $plugin)'
	;;

-h | --help) usage ;;

*)
	echo "Unknown command: $CMD" >&2
	usage
	;;
esac
