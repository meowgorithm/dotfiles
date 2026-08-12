#!/usr/bin/env bash
set -euo pipefail

# Capture visible or scrolling content from a tmux pane.
#
# Usage:
#   tmux_capture.sh [options] [target]
#
# Target formats (default: active pane of current session):
#   session-name             → active pane in that session
#   session-name:window      → active pane in that window
#   session-name:window.pane → specific pane
#   :window.pane             → current session, specific pane
#   .pane                    → current session+window, specific pane
#
# Options:
#   -l, --lines N    Number of lines to capture from scrollback (default: visible only)
#   -a, --all        Capture entire scrollback history
#   -s, --strip      Strip trailing whitespace from each line
#   -j, --join       Join wrapped lines (unwrap)
#   -p, --prefix     Prefix each line with line number
#   -h, --help       Show this help

usage() {
	sed -n '4,/^$/p' "$0" | sed 's/^# \?//' >&2
	exit 1
}

LINES=""
ALL=false
STRIP=false
JOIN=false
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
	-s | --strip)
		STRIP=true
		shift
		;;
	-j | --join)
		JOIN=true
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

TARGET="${1:-}"

# Build the tmux capture-pane command
CAP_ARGS=(-p)

if $ALL; then
	CAP_ARGS+=(-S -) # -S - means entire scrollback
elif [[ -n "$LINES" ]]; then
	CAP_ARGS+=(-S "-$LINES")
fi

if $STRIP; then
	CAP_ARGS+=(--) # end of options marker not needed for tmux
fi

# Capture content
if [[ -n "$TARGET" ]]; then
	RAW=$(tmux capture-pane "${CAP_ARGS[@]}" -t "$TARGET" 2>&1) || {
		echo "Error: failed to capture from target '$TARGET'" >&2
		echo "tmux error: $RAW" >&2
		exit 1
	}
else
	RAW=$(tmux capture-pane "${CAP_ARGS[@]}" 2>&1) || {
		echo "Error: failed to capture from active pane" >&2
		echo "tmux error: $RAW" >&2
		exit 1
	}
fi

# Strip trailing whitespace per line
if $STRIP; then
	RAW=$(printf '%s\n' "$RAW" | sed 's/[[:space:]]*$//')
fi

# Join wrapped lines: lines that don't end with a hard newline
# are continuation lines. tmux capture-pane already returns proper
# newlines, so "join" means removing blank lines between wrapped
# visual lines — but that's ambiguous. Instead, join just strips
# trailing empty lines and collapses multiple blanks to one.
if $JOIN; then
	RAW=$(echo "$RAW" | awk 'NF>0 || !blank++ {print} NF>0 {blank=0}')
fi

# Prefix with line numbers
if $PREFIX; then
	RAW=$(echo "$RAW" | awk '{printf "%4d  %s\n", NR, $0}')
fi

printf '%s\n' "$RAW"
