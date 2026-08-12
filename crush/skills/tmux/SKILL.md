---
name: tmux
description: Drive tmux programmatically — create sessions, send commands to panes, read pane output, manage layouts, and interact with running terminal processes. Use when Crush needs to run something in a terminal and read its output, send keystrokes to a running process (CLI, REPL, editor, server), manage multiple terminal sessions, create persistent dev environments, interact with long-running processes, capture terminal scrollback, send special keys (Ctrl+C, Escape, arrows), split/resize/zoom panes, run background servers alongside a foreground task, or any task that requires interacting with a real TTY.
---

# tmux

Drive tmux from Crush: send commands to panes, read pane output, and manage sessions. This lets Crush interact with any terminal process — REPLs, editors, servers, interactive CLIs.

## Prerequisites

Requires `tmux` on the system. Verify:

```bash
command -v tmux && tmux -V
```

If missing, install: `brew install tmux` (macOS) or `apt-get install tmux` (Linux).

## Quick Start: Run a Command and Read Output

```bash
# 1. Create a session
bash scripts/tmux_session.sh new mywork "$PWD"

# 2. Send a command
bash scripts/tmux_send.sh mywork "echo hello && ls -la"

# 3. Read the output (last 30 lines)
bash scripts/tmux_capture.sh -l 30 mywork

# 4. Send Ctrl+C to stop something
bash scripts/tmux_send.sh -k mywork C-c

# 5. Kill the session when done
bash scripts/tmux_session.sh kill mywork
```

## Target Format

All scripts accept a **target** string:

| Format | Example | Meaning |
|--------|---------|---------|
| `session` | `mywork` | Active pane in session |
| `session:window` | `mywork:1` | Active pane in window |
| `session:window.pane` | `mywork:1.2` | Specific pane |
| `.pane` | `.1` | Pane in current window (if inside tmux) |

For full details, see [references/pane-targeting.md](references/pane-targeting.md).

## Scripts

### `scripts/tmux_capture.sh` — Read Pane Output

Capture visible or scrollback content from a pane.

```bash
# Visible content only (default)
bash scripts/tmux_capture.sh mywork

# Last 50 lines of scrollback
bash scripts/tmux_capture.sh -l 50 mywork

# Entire scrollback
bash scripts/tmux_capture.sh -a mywork

# With line numbers
bash scripts/tmux_capture.sh -p -l 100 mywork

# Strip trailing whitespace
bash scripts/tmux_capture.sh -s -l 50 mywork
```

| Option | Description |
|--------|-------------|
| `-l, --lines N` | Capture N lines of scrollback |
| `-a, --all` | Capture entire scrollback |
| `-s, --strip` | Strip trailing whitespace per line |
| `-j, --join` | Collapse blank lines, join wrapped content |
| `-p, --prefix` | Prefix lines with line numbers |

### `scripts/tmux_send.sh` — Send Commands to Panes

Send text commands or special keys to a pane.

```bash
# Run a command (Enter is sent automatically)
bash scripts/tmux_send.sh mywork "go test ./..."

# Send without Enter (type into a prompt)
bash scripts/tmux_send.sh -n mywork "docker"

# Send special keys
bash scripts/tmux_send.sh -k mywork C-c
bash scripts/tmux_send.sh -k mywork Enter
bash scripts/tmux_send.sh -k mywork Escape
bash scripts/tmux_send.sh -k mywork Up
bash scripts/tmux_send.sh -k mywork C-a C-k

# Send literal text (no key interpretation)
bash scripts/tmux_send.sh -l mywork "C-c is just text here"

# Send and read back output (waits 0.3s, captures 30 lines)
bash scripts/tmux_send.sh -r mywork "echo \$HOME"

# Send and wait longer before reading back
bash scripts/tmux_send.sh -w 3 mywork "sleep 2 && echo done"
```

| Option | Description |
|--------|-------------|
| `-e, --enter` | Send Enter after command (default) |
| `-n, --no-enter` | Do not send Enter |
| `-k, --keys` | Treat arguments as key names (C-c, Escape, Up) |
| `-l, --literal` | Send as literal text, no key interpretation |
| `-d, --delay S` | Wait S seconds before sending |
| `-r, --read-back` | Capture and print pane output after sending |
| `-w, --wait S` | Wait S seconds before read-back (implies -r) |

For all key names (C-c, Escape, F1, arrows, etc.), see [references/key-names.md](references/key-names.md).

### `scripts/tmux_session.sh` — Session/Window/Pane Management

```bash
# Sessions
bash scripts/tmux_session.sh new mywork "$PWD"     # create detached session
bash scripts/tmux_session.sh list                    # full tree: sessions→windows→panes
bash scripts/tmux_session.sh list-sessions          # session names only
bash scripts/tmux_session.sh kill mywork             # kill session
bash scripts/tmux_session.sh exists mywork           # check existence (exit 0/1)
bash scripts/tmux_session.sh attached mywork        # check if attached (exit 0/1)

# Windows
bash scripts/tmux_session.sh new-window mywork "logs" "$PWD"
bash scripts/tmux_session.sh list-windows mywork
bash scripts/tmux_session.sh kill-window mywork:1

# Panes
bash scripts/tmux_session.sh split mywork:0.1 -v "$PWD"   # split vertically (below)
bash scripts/tmux_session.sh split mywork:0.1 -h "$PWD"   # split horizontally (right)
bash scripts/tmux_session.sh list-panes mywork:0
bash scripts/tmux_session.sh kill-pane mywork:0.2
bash scripts/tmux_session.sh select mywork:0.1
bash scripts/tmux_session.sh resize mywork:0.1 -L 20     # resize left by 20
bash scripts/tmux_session.sh zoom mywork:0.1              # toggle zoom
bash scripts/tmux_session.sh break-pane mywork:0.2 mywork # pane → new window

# Info
bash scripts/tmux_session.sh info mywork:0.1
```

## Common Patterns

### Run a long-lived server and check its output

```bash
bash scripts/tmux_session.sh new api "$PWD"
bash scripts/tmux_send.sh -n api "go run ./cmd/api"
sleep 2
bash scripts/tmux_capture.sh -l 50 api
```

### Interact with a REPL (Python, Node, etc.)

```bash
bash scripts/tmux_session.sh new repl "$PWD"
bash scripts/tmux_send.sh -n repl "python3"
sleep 1
bash scripts/tmux_send.sh -r repl "import os; print(os.getcwd())"
bash scripts/tmux_send.sh -k repl C-d  # exit REPL
```

### Send Ctrl+C to stop a running process

```bash
bash scripts/tmux_send.sh -k mywork C-c
```

### Read output, check for a prompt, then send more commands

```bash
# Check if a shell prompt is ready
OUTPUT=$(bash scripts/tmux_capture.sh -l 10 mywork)
if echo "$OUTPUT" | grep -q '\$ $'; then
  bash scripts/tmux_send.sh mywork "next-command"
fi
```

### Multi-pane dev environment

```bash
bash scripts/tmux_session.sh new dev "$PWD"
bash scripts/tmux_session.sh split dev:0.0 -h "$PWD"   # right pane
bash scripts/tmux_session.sh split dev:0.1 -v "$PWD"   # split right into top/bottom

# Pane 0: Crush, Pane 1: server (top-right), Pane 2: CLI (bottom-right)
bash scripts/tmux_send.sh dev:0.1 "hivemind"            # start server
bash scripts/tmux_send.sh dev:0.2 "ls -la"              # run in CLI pane

# Read server logs
bash scripts/tmux_capture.sh -l 30 dev:0.1
```

### Send text to an editor (vi/helix/etc.)

```bash
# Save in helix
bash scripts/tmux_send.sh -k editor Escape ":w" Enter

# Quit vi
bash scripts/tmux_send.sh -k editor Escape ":q!" Enter

# Type text into insert mode
bash scripts/tmux_send.sh -k editor i "hello world" Escape
```

### Monitor a pane until a pattern appears

```bash
for i in $(seq 1 30); do
  OUTPUT=$(bash scripts/tmux_capture.sh -l 20 mywork)
  echo "$OUTPUT" | grep -q "Server started" && break
  sleep 1
done
```

### Paginated output (less, man, etc.)

```bash
# Send Space to advance a page
bash scripts/tmux_send.sh -k mywork Space

# Send 'q' to quit pager
bash scripts/tmux_send.sh -k mywork q

# Search in less
bash scripts/tmux_send.sh -k mywork / "error" Enter
```

## Key Decisions

- **Always create a named session** for tracked work. Unnamed sessions get
  auto-generated IDs that are hard to target.
- **Use `-r` (read-back) sparingly** — it adds latency. Prefer explicit
  `tmux_capture.sh` calls after a known delay.
- **Wait for output before reading.** Most processes need 0.5-2s. Use
  `-w <seconds>` or `sleep` before `tmux_capture.sh`.
- **Ctrl+C is safe.** If a pane is idle, sending `C-c` is a no-op.
- **Check `exists` before operations** on sessions that might have been
  killed externally.
- **Clean up sessions when done** with `kill` to avoid orphaned processes.

## Running Inside tmux

If Crush itself runs inside a tmux pane, `$TMUX` is set and `$TMUX_PANE`
holds the current pane's ID. Scripts work the same way — just use
explicit session names rather than relying on the current session.
