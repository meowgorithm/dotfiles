#!/usr/bin/env bash
set -euo pipefail

# Capture visible or scrollback content from a zellij pane.
#
# Usage:
#   zellij_capture.sh [options] <session> [pane]
#
# Pane formats (default: focused terminal pane, else first terminal pane):
#   terminal_1   → full pane id
#   3            → shorthand for terminal_3
#   plugin_2     → plugin pane
#
# Options:
#   -l, --lines N    Last N lines (dumps full scrollback, then tails)
#   -a, --all        Entire scrollback
#   -A, --ansi       Preserve ANSI escape sequences (colors, styles)
#   -s, --strip      Strip trailing whitespace from each line
#   -p, --prefix     Prefix each line with line number
#   -h, --help       Show this help

usage() {
	sed -n '4,/^$/p' "$0" | sed 's/^# \?//' >&2
	exit 1
}

LINES=""
ALL=false
ANSI=false
STRIP=false
PREFIX=false

while [[ $# -gt 0 ]]; do
	case "$1" in
	-l | --lines)
		LINES="$2"
		shift 2
		;;
	-a | --all)
		ALL=true
		shift
		;;
	-A | --ansi)
		ANSI=true
		shift
		;;
	-s | --strip)
		STRIP=true
		shift
		;;
	-p | --prefix)
		PREFIX=true
		shift
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

SESSION="${1:-}"
PANE="${2:-}"
[[ -z "$SESSION" ]] && {
	echo "Error: session required" >&2
	usage
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

PANE_ID=$(resolve_pane "$SESSION" "$PANE")

DUMP_ARGS=(--pane-id "$PANE_ID")
if $ALL || [[ -n "$LINES" ]]; then
	DUMP_ARGS+=(--full)
fi
if $ANSI; then
	DUMP_ARGS+=(--ansi)
fi

RAW=$(zellij --session "$SESSION" action dump-screen "${DUMP_ARGS[@]}" 2>&1) || {
	echo "Error: failed to capture pane '$PANE_ID' in session '$SESSION'" >&2
	echo "zellij error: $RAW" >&2
	exit 1
}

if $STRIP; then
	RAW=$(printf '%s\n' "$RAW" | sed 's/[[:space:]]*$//')
fi

if [[ -n "$LINES" ]]; then
	RAW=$(printf '%s\n' "$RAW" | tail -n "$LINES")
fi

if $PREFIX; then
	RAW=$(printf '%s\n' "$RAW" | awk '{printf "%4d  %s\n", NR, $0}')
fi

printf '%s\n' "$RAW"
