#!/usr/bin/env bash
set -euo pipefail

# Manage tmux sessions, windows, and panes.
#
# Usage:
#   tmux_session.sh <command> [options]
#
# Commands:
#   new [name] [workdir]              Create new session (optionally named, in workdir)
#   kill [session]                    Kill session
#   list                              List sessions with windows and panes
#   list-sessions                      List session names only
#   list-windows [session]             List windows in a session
#   list-panes [session:window]       List panes with index, title, command
#   split [target] [-h|-v] [workdir]   Split pane (horizontal or vertical)
#   new-window [session] [name] [workdir]  Create new window
#   kill-window [session:window]       Kill window
#   kill-pane [target]                 Kill a pane
#   select [target]                    Select a pane/window/session
#   rename [target] [name]             Rename a session or window
#   resize [target] [-L|-R|-U|-D] [n]  Resize pane by n cells
#   zoom [target]                      Toggle zoom on a pane
#   break-pane [target] [session]      Break pane into its own window
#   info [target]                      Show detailed info about a pane
#   exists [session]                   Check if session exists (exit 0/1)
#   attached [session]                 Check if session is attached (exit 0/1)
#   -h, --help                         Show this help

usage() {
	sed -n '4,/^$/p' "$0" | sed 's/^# \?//' >&2
	exit 1
}

[[ $# -lt 1 ]] && usage

CMD="$1"
shift

case "$CMD" in
new)
	NAME="${1:-}"
	WORKDIR="${2:-.}"
	ARGS=(new-session -d -c "$WORKDIR")
	[[ -n "$NAME" ]] && ARGS+=(-s "$NAME")
	SESSION_ID=$(tmux "${ARGS[@]}" 2>&1) || {
		# If session exists, attach or list
		if [[ -n "$NAME" ]] && tmux has-session -t "$NAME" 2>/dev/null; then
			echo "Session '$NAME' already exists"
			echo "$NAME"
			exit 0
		fi
		echo "Error creating session: $SESSION_ID" >&2
		exit 1
	}
	# Print the session name (or ID if unnamed)
	if [[ -n "$NAME" ]]; then
		echo "$NAME"
	else
		tmux display-message -p -t "$(tmux display-message -p '#{session_id}')" '#{session_name}'
	fi
	;;

kill)
	SESSION="${1:-}"
	[[ -z "$SESSION" ]] && {
		echo "Error: session name required" >&2
		exit 1
	}
	tmux kill-session -t "$SESSION" 2>/dev/null && echo "killed $SESSION" || echo "no such session: $SESSION" >&2
	;;

list)
	# Full tree: sessions → windows → panes
	tmux list-sessions -F "#{session_name}" 2>/dev/null | sort | while read -r sess; do
		echo "=== session: $sess ==="
		tmux list-windows -t "$sess" -F "  window: #{window_index} '#{window_name}' (#{window_panes} panes) #{window_active}" 2>/dev/null | while read -r win; do
			echo "$win"
			# Extract window index from the line
			widx=$(echo "$win" | awk '{print $2}' | tr -d ':')
			tmux list-panes -t "${sess}:${widx}" -F "    pane: #{pane_index} id=#{pane_id} #{pane_current_command} '#{pane_title}' #{pane_active} #{pane_width}x#{pane_height}" 2>/dev/null | while read -r pane; do
				echo "$pane"
			done
		done
	done
	;;

list-sessions)
	tmux list-sessions -F "#{session_name}" 2>/dev/null || echo "(no sessions)"
	;;

list-windows)
	SESSION="${1:-}"
	if [[ -z "$SESSION" ]]; then
		tmux list-windows -F "#{window_index} '#{window_name}' (#{window_panes} panes)"
	else
		tmux list-windows -t "$SESSION" -F "#{window_index} '#{window_name}' (#{window_panes} panes)"
	fi
	;;

list-panes)
	TARGET="${1:-}"
	if [[ -z "$TARGET" ]]; then
		tmux list-panes -F "#{pane_index} #{pane_id} #{pane_current_command} '#{pane_title}' active=#{pane_active}"
	else
		tmux list-panes -t "$TARGET" -F "#{pane_index} #{pane_id} #{pane_current_command} '#{pane_title}' active=#{pane_active}"
	fi
	;;

split)
	TARGET="${1:-}"
	shift 2>/dev/null || true
	DIRECTION="${1:--v}"
	WORKDIR="${2:-}"
	[[ -z "$TARGET" ]] && {
		echo "Error: target required" >&2
		exit 1
	}
	ARGS=(split-window -t "$TARGET")
	case "$DIRECTION" in
	-h | horizontal) ARGS+=(-h) ;;
	-v | vertical) ARGS+=(-v) ;;
	*) ARGS+=(-v) ;; # default vertical
	esac
	[[ -n "$WORKDIR" ]] && ARGS+=(-c "$WORKDIR")
	PANE_INFO=$(tmux "${ARGS[@]}" -P -F '#{session_name}:#{window_index}.#{pane_index}' 2>&1) || {
		echo "Error splitting pane: $PANE_INFO" >&2
		exit 1
	}
	echo "$PANE_INFO"
	;;

new-window)
	SESSION="${1:-}"
	NAME="${2:-}"
	WORKDIR="${3:-.}"
	ARGS=(new-window)
	[[ -n "$SESSION" ]] && ARGS+=(-t "$SESSION")
	[[ -n "$NAME" ]] && ARGS+=(-n "$NAME")
	ARGS+=(-c "$WORKDIR")
	WIN_INFO=$(tmux "${ARGS[@]}" -P -F '#{session_name}:#{window_index}' 2>&1) || {
		echo "Error creating window: $WIN_INFO" >&2
		exit 1
	}
	echo "$WIN_INFO"
	;;

kill-window)
	TARGET="${1:-}"
	[[ -z "$TARGET" ]] && {
		echo "Error: target required" >&2
		exit 1
	}
	tmux kill-window -t "$TARGET" 2>/dev/null && echo "killed $TARGET" || echo "no such window: $TARGET" >&2
	;;

kill-pane)
	TARGET="${1:-}"
	[[ -z "$TARGET" ]] && {
		echo "Error: target required" >&2
		exit 1
	}
	tmux kill-pane -t "$TARGET" 2>/dev/null && echo "killed $TARGET" || echo "no such pane: $TARGET" >&2
	;;

select)
	TARGET="${1:-}"
	[[ -z "$TARGET" ]] && {
		echo "Error: target required" >&2
		exit 1
	}
	tmux select-pane -t "$TARGET" 2>/dev/null ||
		tmux select-window -t "$TARGET" 2>/dev/null ||
		tmux switch-client -t "$TARGET" 2>/dev/null ||
		{
			echo "Error: could not select '$TARGET'" >&2
			exit 1
		}
	echo "selected $TARGET"
	;;

rename)
	TARGET="${1:-}"
	NAME="${2:-}"
	[[ -z "$TARGET" || -z "$NAME" ]] && {
		echo "Error: target and name required" >&2
		exit 1
	}
	# Try session rename, fall back to window rename
	tmux rename-session -t "$TARGET" "$NAME" 2>/dev/null ||
		tmux rename-window -t "$TARGET" "$NAME" 2>/dev/null ||
		{
			echo "Error: could not rename '$TARGET'" >&2
			exit 1
		}
	echo "renamed $TARGET → $NAME"
	;;

resize)
	TARGET="${1:-}"
	DIR="${2:--L}"
	AMOUNT="${3:-5}"
	[[ -z "$TARGET" ]] && {
		echo "Error: target required" >&2
		exit 1
	}
	tmux resize-pane -t "$TARGET" "$DIR" "$AMOUNT" 2>/dev/null || {
		echo "Error resizing pane" >&2
		exit 1
	}
	echo "resized $TARGET $DIR $AMOUNT"
	;;

zoom)
	TARGET="${1:-}"
	[[ -z "$TARGET" ]] && TARGET=""
	tmux resize-pane -Z -t "${TARGET:-}" 2>/dev/null || true
	echo "zoom toggled"
	;;

break-pane)
	TARGET="${1:-}"
	SESSION="${2:-}"
	[[ -z "$TARGET" ]] && {
		echo "Error: target required" >&2
		exit 1
	}
	ARGS=(break-pane -t "$TARGET")
	[[ -n "$SESSION" ]] && ARGS+=(-s "$SESSION")
	WIN_INFO=$(tmux "${ARGS[@]}" -P -F '#{session_name}:#{window_index}' 2>&1) || {
		echo "Error breaking pane: $WIN_INFO" >&2
		exit 1
	}
	echo "$WIN_INFO"
	;;

info)
	TARGET="${1:-}"
	if [[ -z "$TARGET" ]]; then
		tmux display-message -p "session=#{session_name} window=#{window_index}:#{window_name} pane=#{pane_index}:#{pane_id} cmd=#{pane_current_command} title=#{pane_title} size=#{pane_width}x#{pane_height} active=#{pane_active} path=#{pane_current_path}"
	else
		tmux display-message -t "$TARGET" -p "session=#{session_name} window=#{window_index}:#{window_name} pane=#{pane_index}:#{pane_id} cmd=#{pane_current_command} title=#{pane_title} size=#{pane_width}x#{pane_height} active=#{pane_active} path=#{pane_current_path}"
	fi
	;;

exists)
	SESSION="${1:-}"
	[[ -z "$SESSION" ]] && {
		echo "Error: session name required" >&2
		exit 1
	}
	tmux has-session -t "$SESSION" 2>/dev/null
	;;

attached)
	SESSION="${1:-}"
	[[ -z "$SESSION" ]] && {
		echo "Error: session name required" >&2
		exit 1
	}
	COUNT=$(tmux list-sessions -t "$SESSION" -F '#{session_attached}' 2>/dev/null || echo "0")
	[[ "$COUNT" -gt 0 ]] 2>/dev/null
	;;

-h | --help) usage ;;

*)
	echo "Unknown command: $CMD" >&2
	usage
	;;
esac
