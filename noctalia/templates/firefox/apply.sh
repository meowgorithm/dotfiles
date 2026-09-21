#!/usr/bin/env bash
set -euo pipefail

if command -v flock >/dev/null 2>&1; then
    exec 9>"${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}/noctalia-firefox-apply.lock"
    flock -w 15 9 || exit 0
fi

cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}"
css_chrome="$cache_dir/noctalia/firefox/userChrome.css"
css_content="$cache_dir/noctalia/firefox/userContent.css"
line_chrome="@import \"$css_chrome\";"
line_content="@import \"$css_content\";"

write_if_changed() {
    local target="$1" tmp="$2"
    if ! cmp -s "$target" "$tmp"; then
        cat "$tmp" >"$target"
    fi
    rm -f "$tmp"
}

firefox_dirs=()
for d in \
    "${XDG_CONFIG_HOME:-$HOME/.config}/mozilla/firefox" \
    "$HOME/.mozilla/firefox" \
    "$HOME/.var/app/org.mozilla.firefox/.config/mozilla/firefox" \
    "$HOME/.var/app/org.mozilla.firefox/.mozilla/firefox"; do
    [ -d "$d" ] && firefox_dirs+=("$d")
done
[ "${#firefox_dirs[@]}" -eq 0 ] && exit 0

find "${firefox_dirs[@]}" -mindepth 2 -maxdepth 2 -type f -name "prefs.js" -print0 |
    while IFS= read -r -d '' prefs_file; do
        profile_dir=$(dirname "$prefs_file")
        chrome_dir="$profile_dir/chrome"
        user_chrome="$chrome_dir/userChrome.css"
        user_content="$chrome_dir/userContent.css"
        user_js="$profile_dir/user.js"

        mkdir -p "$chrome_dir"
        touch "$user_chrome" "$user_content" "$user_js"

        tmp_chrome="$(mktemp "${user_chrome}.tmp.XXXXXX")"
        printf '%s\n' "$line_chrome" >"$tmp_chrome"
        sed '/noctalia\/firefox\/userChrome\.css/d' "$user_chrome" >>"$tmp_chrome"
        write_if_changed "$user_chrome" "$tmp_chrome"

        tmp_content="$(mktemp "${user_content}.tmp.XXXXXX")"
        printf '%s\n' "$line_content" >"$tmp_content"
        sed '/noctalia\/firefox\/userContent\.css/d' "$user_content" >>"$tmp_content"
        write_if_changed "$user_content" "$tmp_content"

        tmp_js="$(mktemp "${user_js}.tmp.XXXXXX")"
        sed \
            -e '/toolkit\.legacyUserProfileCustomizations\.stylesheets/d' \
            "$user_js" >"$tmp_js"
        [ -s "$tmp_js" ] && [ -n "$(tail -c1 "$tmp_js")" ] && echo >>"$tmp_js"
        printf '%s\n' \
            'user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);' >>"$tmp_js"
        write_if_changed "$user_js" "$tmp_js"
    done
