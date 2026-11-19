#!/usr/bin/env bash
# Ping Grok Bot when a Crush turn ends.
# Reads GROK_WEBHOOK_URL / GROK_WEBHOOK_KEY (or GROKBOT_*) from the environment
# only. Set them in your private env; nothing secret lives in this repo.
url="${GROK_WEBHOOK_URL:-$GROKBOT_WEBHOOK_URL}"
key="${GROK_WEBHOOK_KEY:-$GROKBOT_WEBHOOK_KEY}"
[ -n "$url" ] && [ -n "$key" ] || exit 0
# Accept a bare key or a full "Authorization: Bearer <key>" value.
key="${key#Authorization:}"; key="${key#"${key%%[![:space:]]*}"}"
key="${key#Bearer}"; key="${key#"${key%%[![:space:]]*}"}"
payload=$(cat)
host=$(hostname -s)
payload=$(printf %s "$payload" | python3 -c 'import json,sys;d=json.load(sys.stdin);d["host"]=sys.argv[1];print(json.dumps(d))' "$host" 2>/dev/null || printf %s "$payload")
curl -s --max-time 10 -X POST "$url" \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $key" \
  --data "$payload" >/dev/null 2>&1 &
exit 0
