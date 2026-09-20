#!/usr/bin/env bash
set -euo pipefail

# Send commands, keys, bytes, or pasted text to a zellij pane.
#
# Usage:
#   zellij_send.sh [options] <session> [pane] <text...>
#   zellij_send.sh [options] <session> [pane] -- <text...>
#
# Pane formats (default: focused terminal pane, else first terminal pane):
#   terminal_1   → full pane id
#   3            → shorthand for terminal_3
#   plugin_2     → plugin pane
# The pane argument is optional; it is only recognized if it matches a pane
# id pattern. Use `--` to force everything after it to be treated as text.
#
# Options:
#   -e, --enter        Send Enter after the text (default)
#   -n, --no-enter     Do not send Enter after the text
#   -k, --keys         Treat arguments as key names ("Ctrl c", "Esc", "Up")
#   -b, --bytes        Treat arguments as raw byte values (write action).
#                      In bytes mode only full pane ids are recognized.
#   -P, --paste        Paste text via bracketed paste (safe for multiline,
#                      never auto-sends Enter)
#   -d, --delay SEC    Wait SEC seconds before sending
#   -r, --read-back    Capture and print pane output after sending (0.3s delay)
#   -w, --wait SEC     Wait SEC seconds before read-back (implies --read-back)
#   -h, --help         Show this help

usage() {
	sed -n '4,/^$/p' "$0" | sed 's/^# \?//' >&2
	exit 1
}

ENTER=true
KEYS=false
BYTES=false
PASTE=false
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
	-b | --bytes)
		BYTES=true
		shift
		;;
	-P | --paste)
		PASTE=true
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
	echo "Error: session required" >&2
	usage
}
SESSION="$1"
shift

[[ $# -lt 1 ]] && {
	echo "Error: text or keys required" >&2
	usage
}

# Optional pane argument. In bytes mode, bare numbers are bytes, so only
# full pane ids are recognized there.
PANE=""
if [[ $# -gt 1 ]]; then
	if $BYTES; then
		case "$1" in
		terminal_* | plugin_*)
			PANE="$1"
			shift
			;;
		esac
	elif [[ "$1" =~ ^(terminal_[0-9]+|plugin_[0-9]+|[0-9]+)$ ]]; then
		PANE="$1"
		shift
	fi
fi

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

PANE_ID=$(resolve_pane "$SESSION" "$PANE")

if (($(awk -v d="$DELAY" 'BEGIN {print (d+0) > 0}'))); then
	sleep "$DELAY"
fi

if $KEYS; then
	zellij --session "$SESSION" action send-keys --pane-id "$PANE_ID" "$@"
elif $BYTES; then
	zellij --session "$SESSION" action write --pane-id "$PANE_ID" "$@"
elif $PASTE; then
	zellij --session "$SESSION" action paste --pane-id "$PANE_ID" "$*"
else
	zellij --session "$SESSION" action write-chars --pane-id "$PANE_ID" "$*"
	if $ENTER; then
		zellij --session "$SESSION" action send-keys --pane-id "$PANE_ID" Enter
	fi
fi

if $READBACK; then
	if [[ "$WAIT" != "0" ]]; then
		sleep "$WAIT"
	else
		sleep 0.3
	fi
	SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
	bash "$SCRIPT_DIR/zellij_capture.sh" -l 30 "$SESSION" "$PANE_ID"
fi
