#!/usr/bin/env bash
# Ping Crush Control when a Crush turn ends.
# Expects GROKBOT_WEBHOOK_URL and GROKBOT_WEBHOOK_KEY in the environment.
[ -n "$GROKBOT_WEBHOOK_URL" ] || exit 0
payload=$(cat)
host=$(hostname -s)
payload=$(printf %s "$payload" | python3 -c 'import json,sys;d=json.load(sys.stdin);d["host"]=sys.argv[1];print(json.dumps(d))' "$host" 2>/dev/null || printf %s "$payload")
curl -s --max-time 10 -X POST "$GROKBOT_WEBHOOK_URL" \
    -H 'Content-Type: application/json' \
    -H "Authorization: Bearer $GROKBOT_WEBHOOK_KEY" \
    --data "$payload" >/dev/null 2>&1 &
exit 0
