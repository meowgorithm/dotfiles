#!/usr/bin/env bash
set -euo pipefail

if command -v flock >/dev/null 2>&1; then
    exec 9>"${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}/noctalia-thunar-apply.lock"
    flock -w 15 9 || exit 0
fi

cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}"
css_thunar="$cache_dir/noctalia/thunar/gtk.css"
gtk_dir="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-3.0"
gtk_css="$gtk_dir/gtk.css"
line="@import \"$css_thunar\";"

mkdir -p "$gtk_dir"
touch "$gtk_css"

tmp="$(mktemp "${gtk_css}.tmp.XXXXXX")"
printf '%s\n' "$line" >"$tmp"
sed '/noctalia\/thunar\/gtk\.css/d' "$gtk_css" >>"$tmp"
if ! cmp -s "$gtk_css" "$tmp"; then
    cat "$tmp" >"$gtk_css"
fi
rm -f "$tmp"
