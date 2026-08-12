#!/usr/bin/env bash
set -euo pipefail

# Send commands or keys to a tmux pane.
#
# Usage:
#   tmux_send.sh [options] <target> <command>
#   tmux_send.sh [options] <target> -- <command...>
#
# Target formats:
#   session-name             → active pane in that session
#   session-name:window      → active pane in that window
#   session-name:window.pane → specific pane
#   :window.pane             → current session, specific pane
#   .pane                    → current session+window, specific pane
#
# Options:
#   -e, --enter        Send Enter key after the command (default: true)
#   -n, --no-enter     Do not send Enter after the command
#   -k, --keys          Treat <command> as literal key name(s) (e.g. C-c, Escape, Up)
#   -l, --literal       Send as literal text (no tmux key interpretation)
#   -d, --delay SECONDS Wait SECONDS before sending (useful for slow panes)
#   -r, --read-back     Capture and print pane output after sending (with 0.3s delay)
#   -w, --wait SECONDS  Wait SECONDS before read-back (implies --read-back)
#   -h, --help          Show this help

usage() {
	sed -n '4,/^$/p' "$0" | sed 's/^# \?//' >&2
	exit 1
}

ENTER=true
KEYS=false
LITERAL=false
DELAY=0
READBACK=false
WAIT=0

while [[ $# -gt 0 ]]; do
	case "$1" in
	-e | --enter)
		ENTER=true
		shift
		;;
	-n | --no-enter)
		ENTER=false
		shift
		;;
	-k | --keys)
		KEYS=true
		shift
		;;
	-l | --literal)
		LITERAL=true
		shift
		;;
	-d | --delay)
		DELAY="$2"
		shift 2
		;;
	-r | --read-back)
		READBACK=true
		shift
		;;
	-w | --wait)
		WAIT="$2"
		READBACK=true
		shift 2
		;;
	-h | --help) usage ;;
	--)
		shift
		break
		;;
	-*)
		echo "Unknown option: $1" >&2
		usage
		;;
	*) break ;;
	esac
done

[[ $# -lt 1 ]] && {
	echo "Error: target required" >&2
	usage
}
TARGET="$1"
shift

if [[ $# -lt 1 ]]; then
	echo "Error: command or keys required" >&2
	usage
fi

# Collect the command (may be multi-word after --)
CMD="$*"

# Optional pre-delay
if (($(echo "$DELAY" | awk '{print ($1+0)>0}'))); then
	sleep "$DELAY"
fi

# Build send-keys args
SK_ARGS=(-t "$TARGET")

if $LITERAL; then
	# -l sends literal text without interpreting key names
	SK_ARGS+=(-l "$CMD")
elif $KEYS; then
	# Each token is a key name (C-c, Escape, Up, etc.)
	for k in "$@"; do
		SK_ARGS+=("$k")
	done
else
	# Default: send the command as text, then optionally Enter
	SK_ARGS+=("$CMD")
	if $ENTER; then
		SK_ARGS+=("Enter")
	fi
fi

tmux send-keys "${SK_ARGS[@]}"

# Read back output if requested
if $READBACK; then
	if [[ "$WAIT" -gt 0 ]] 2>/dev/null; then
		sleep "$WAIT"
	else
		sleep 0.3
	fi
	SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
	bash "$SCRIPT_DIR/tmux_capture.sh" -l 30 "$TARGET"
fi
