---
name: zellij
description: Drive zellij programmatically — create sessions, send commands to panes, read pane output (plain or with ANSI colors/styles preserved), manage tabs and layouts, and interact with running terminal processes. Use when Crush needs to run something in a terminal and read its output, see terminal colors or styling (ANSI escape sequences), send keystrokes to a running process (CLI, REPL, editor, server), manage persistent dev environments, interact with long-running processes, capture terminal scrollback, send special keys (Ctrl+C, Esc, arrows), split panes or tabs, or any task that requires interacting with a real TTY via zellij.
---

# zellij

Drive zellij from Crush: send commands to panes, read pane output (optionally
with ANSI colors and styles intact), and manage sessions, tabs, and panes.
This lets Crush interact with any terminal process — REPLs, editors, servers,
interactive CLIs.

## Prerequisites

Requires `zellij` (developed against 0.45; `--pane-id` targeting and
`dump-screen --ansi` need a recent version) and `jq` (pane discovery). Verify:

```bash
command -v zellij && zellij --version && command -v jq
```

If missing, install: `brew install zellij jq` (macOS) or
`apt-get install zellij jq` (Linux).

## Quick Start: Run a Command and Read Output

```bash
# 1. Create a detached session
bash scripts/zellij_session.sh new mywork "$PWD"

# 2. Send a command (auto-targets the focused terminal pane)
bash scripts/zellij_send.sh mywork "echo hello && ls -la"

# 3. Read the output (last 30 lines, plain text)
bash scripts/zellij_capture.sh -l 30 mywork

# 4. Read it again with colors and styles preserved
bash scripts/zellij_capture.sh -l 30 --ansi mywork

# 5. Send Ctrl+C to stop something
bash scripts/zellij_send.sh -k mywork "Ctrl c"

# 6. Kill the session when done
bash scripts/zellij_session.sh kill mywork
```

## Target Format

All scripts take a **session** name plus an optional **pane**:

| Format | Example | Meaning |
|--------|---------|---------|
| `session` | `mywork` | Focused terminal pane (else first terminal pane) |
| `session N` | `mywork 2` | Pane `terminal_2` |
| `session terminal_N` | `mywork terminal_2` | Same, explicit |
| `session plugin_N` | `mywork plugin_1` | A plugin pane |

Zellij pane IDs (`terminal_0`, `terminal_3`, ...) are the primary handle.
Discover them with `zellij_session.sh list` or `list-panes --json`.
For full details, see [references/pane-targeting.md](references/pane-targeting.md).

## Scripts

### `scripts/zellij_capture.sh` — Read Pane Output

Capture the visible viewport or scrollback of a pane, plain or with ANSI
styling preserved.

```bash
# Visible viewport only (default)
bash scripts/zellij_capture.sh mywork

# Last 50 lines of scrollback
bash scripts/zellij_capture.sh -l 50 mywork

# Entire scrollback
bash scripts/zellij_capture.sh -a mywork

# Preserve ANSI colors/styles
bash scripts/zellij_capture.sh -l 50 --ansi mywork

# With line numbers, trailing whitespace stripped
bash scripts/zellij_capture.sh -p -s -l 100 mywork terminal_2
```

| Option | Description |
|--------|-------------|
| `-l, --lines N` | Last N lines (dumps scrollback, then tails) |
| `-a, --all` | Entire scrollback |
| `-A, --ansi` | Preserve ANSI escape sequences (colors, bold, ...) |
| `-s, --strip` | Strip trailing whitespace per line |
| `-p, --prefix` | Prefix lines with line numbers |

### `scripts/zellij_send.sh` — Send Commands to Panes

Send text, special keys, raw bytes, or bracketed-paste text to a pane.

```bash
# Run a command (Enter is sent automatically)
bash scripts/zellij_send.sh mywork "go test ./..."

# Type into a prompt without Enter
bash scripts/zellij_send.sh -n mywork "docker"

# Send special keys (zellij key syntax)
bash scripts/zellij_send.sh -k mywork "Ctrl c"
bash scripts/zellij_send.sh -k mywork Enter
bash scripts/zellij_send.sh -k mywork Esc
bash scripts/zellij_send.sh -k mywork Up
bash scripts/zellij_send.sh -k mywork "Ctrl a" "Ctrl k"

# Send raw bytes
bash scripts/zellij_send.sh -b mywork 3        # same as Ctrl+C
bash scripts/zellij_send.sh -b mywork 27       # raw ESC byte

# Bracketed paste (safe multiline, never auto-executes)
bash scripts/zellij_send.sh -P mywork $'line1\nline2'

# Send and read back output (waits 0.3s, captures 30 lines)
bash scripts/zellij_send.sh -r mywork "echo \$HOME"

# Send and wait longer before reading back
bash scripts/zellij_send.sh -w 3 mywork "sleep 2 && echo done"
```

| Option | Description |
|--------|-------------|
| `-e, --enter` | Send Enter after text (default) |
| `-n, --no-enter` | Do not send Enter |
| `-k, --keys` | Arguments are key names ("Ctrl c", "Esc", "Up") |
| `-b, --bytes` | Arguments are raw byte values |
| `-P, --paste` | Bracketed paste; safe for multiline, no auto-Enter |
| `-d, --delay S` | Wait S seconds before sending |
| `-r, --read-back` | Capture and print pane output after sending |
| `-w, --wait S` | Wait S seconds before read-back (implies -r) |

For all key names ("Ctrl c", "Esc", "F1", arrows, etc.), see
[references/key-names.md](references/key-names.md).

### `scripts/zellij_session.sh` — Session/Tab/Pane Management

```bash
# Sessions
bash scripts/zellij_session.sh new mywork "$PWD"   # create detached session
bash scripts/zellij_session.sh list                   # tree: sessions→tabs→panes
bash scripts/zellij_session.sh list-sessions         # session names only
bash scripts/zellij_session.sh kill mywork            # kill (resurrectable)
bash scripts/zellij_session.sh delete mywork         # kill + purge state
bash scripts/zellij_session.sh exists mywork          # exit 0/1
bash scripts/zellij_session.sh attached mywork       # has clients? exit 0/1

# Tabs
bash scripts/zellij_session.sh new-tab mywork "logs"  # prints tab id
bash scripts/zellij_session.sh list-tabs mywork
bash scripts/zellij_session.sh go-to-tab mywork 2        # by index or name
bash scripts/zellij_session.sh rename-tab mywork 1 "api" # by stable tab id
bash scripts/zellij_session.sh close-tab mywork 1

# Panes
bash scripts/zellij_session.sh split mywork right "api" "$PWD"  # prints pane id
bash scripts/zellij_session.sh split mywork down                # split below
bash scripts/zellij_session.sh run mywork job /tmp bash -c 'make; echo done'
bash scripts/zellij_session.sh list-panes mywork --json
bash scripts/zellij_session.sh kill-pane mywork terminal_2
bash scripts/zellij_session.sh focus mywork terminal_1
bash scripts/zellij_session.sh rename-pane mywork terminal_1 "server"
bash scripts/zellij_session.sh resize mywork terminal_1 increase right
bash scripts/zellij_session.sh clear mywork terminal_1        # clear scrollback
bash scripts/zellij_session.sh fullscreen mywork terminal_1  # toggle zoom
bash scripts/zellij_session.sh info mywork terminal_1        # full JSON details
```

Note: `run` executes the command directly, without a shell. Use
`bash -c '...'` for pipes, redirects, or compound commands.

## Reading ANSI Output

`zellij_capture.sh --ansi` preserves the exact styling of each styled run of
text, so Crush can see colors, bold/italic/underline, and other SGR attributes
exactly as a user would. Use it when the *meaning is in the styling*:
checking whether a TUI rendered, verifying color choices, reading
error/warning coloring, distinguishing diff additions (green) from removals
(red), or confirming theme output.

```bash
bash scripts/zellij_capture.sh -l 30 --ansi mywork
```

Example output fragment (from `printf '\033[1;31mRED\033[0m\n'`):

```
^[[31m^[[49m^[[29m^[[28m^[[27m^[[25m^[[25m^[[1m^[[24m^[[22m^[[1m^[[23mRED^[[m
```

How to read it:

- `ESC[38;2;R;G;Bm` truecolor foreground, `ESC[48;2;R;G;Bm` background
- `ESC[38;5;Nm` / `ESC[48;5;Nm` 256-color palette foreground/background
- `ESC[30-37m`, `ESC[90-97m` basic foreground colors (31=red, 32=green, ...)
- `ESC[1m` bold, `ESC[3m` italic, `ESC[4m` underline, `ESC[9m` strikethrough
- `ESC[m` (or `ESC[0m`) resets all attributes

Caveats:

- **Verbose by design.** Zellij re-emits the full SGR state before every
  styled segment, so expect long prefixes (the stack of `49m 29m 28m...`
  resets above is normal). Bound output with `-l` and consider `-s`.
- **Plain is the default.** Use `--ansi` deliberately; plain dumps are far
  easier to read and sufficient for logs and command output.
- Line wrapping follows pane width in both modes.

## Common Patterns

### Run a long-lived server and check its output

```bash
bash scripts/zellij_session.sh new api "$PWD"
PANE=$(bash scripts/zellij_session.sh run api server "$PWD" bash -c 'go run ./cmd/api')
sleep 2
bash scripts/zellij_capture.sh -l 50 api "$PANE"
```

### Interact with a REPL (Python, Node, etc.)

```bash
bash scripts/zellij_session.sh new repl "$PWD"
bash scripts/zellij_send.sh repl "python3"
sleep 1
bash scripts/zellij_send.sh -r repl "import os; print(os.getcwd())"
bash scripts/zellij_send.sh -k repl "Ctrl d"   # exit REPL
```

### Paste multiline code into a REPL safely

```bash
bash scripts/zellij_send.sh -P repl $'def f(x):\n    return x * 2\n'
bash scripts/zellij_send.sh -k repl Enter Enter
```

Bracketed paste prevents premature execution on newlines.

### Send Ctrl+C to stop a running process

```bash
bash scripts/zellij_send.sh -k mywork "Ctrl c"
```

### Read output, check for a prompt, then send more commands

```bash
OUTPUT=$(bash scripts/zellij_capture.sh -l 10 mywork)
if echo "$OUTPUT" | grep -q '\$ $'; then
  bash scripts/zellij_send.sh mywork "next-command"
fi
```

### Multi-pane dev environment

```bash
bash scripts/zellij_session.sh new dev "$PWD"
SERVER=$(bash scripts/zellij_session.sh split dev right "server")
CLI=$(bash scripts/zellij_session.sh split dev down "cli")

bash scripts/zellij_send.sh dev "$SERVER" "hivemind"   # start server
bash scripts/zellij_send.sh dev "$CLI" "ls -la"        # run in CLI pane

bash scripts/zellij_capture.sh -l 30 dev "$SERVER"     # read server logs
```

### Verify a TUI or colorized output rendered correctly

```bash
bash scripts/zellij_capture.sh --ansi -l 40 mywork | grep -q $'\033\[32m' \
  && echo "green (success) styling present"
```

### Send text to an editor (vi/helix/etc.)

```bash
# Save in helix
bash scripts/zellij_send.sh -k editor Esc ":w" Enter

# Quit vi
bash scripts/zellij_send.sh -k editor Esc ":q!" Enter

# Type text in insert mode
bash scripts/zellij_send.sh -k editor i "hello world" Esc
```

### Monitor a pane until a pattern appears

```bash
for i in $(seq 1 30); do
  OUTPUT=$(bash scripts/zellij_capture.sh -l 20 mywork)
  echo "$OUTPUT" | grep -q "Server started" && break
  sleep 1
done
```

### Paginated output (less, man, etc.)

```bash
bash scripts/zellij_send.sh -k mywork Space      # advance a page
bash scripts/zellij_send.sh -k mywork q          # quit pager
bash scripts/zellij_send.sh -k mywork / "error" Enter   # search in less
```

### Attach a human to the session

Sessions created headless can be opened in a real terminal at any time:

```bash
zellij attach mywork
```

Attaching also gives panes a real (larger) geometry; see below.

## Key Decisions and Caveats

- **Always named sessions.** Unnamed sessions get random names that are hard
  to target. `new` is idempotent and prints the name.
- **Pane IDs over focus.** Headless sessions (no attached client) have no
  reliable "focused pane", so every script resolves explicit pane IDs.
  Omitting the pane picks the focused terminal pane, else the first one.
- **Headless geometry is small.** With no client attached, panes render at a
  small default size (observed 50x48 for the first pane, shrinking as you
  split), and dump output wraps at that width. Attach a client
  (`zellij attach <name>`) for real terminal sizes.
- **`run` has no shell.** `zellij run -- cmd` executes `cmd` directly. Use
  `bash -c '...'` for compound commands. Exited `run` panes stay open and
  report `exit_status` via `info`.
- **`kill` vs `delete`.** `kill-session` leaves resurrectable state;
  `delete-session` purges it. Use `delete` for full cleanup.
- **`list-sessions` exits 1** when no sessions exist; guard with `|| true`
  in scripts you write yourself.
- **Tab focus is per-client.** `go-to-tab` affects attached clients; in a
  headless session there is no current tab (`current-tab-info` errors).
  Tabs, renaming, and listing still work headless.
- **Use `-r` (read-back) sparingly** — it adds latency. Prefer explicit
  `zellij_capture.sh` calls after a known delay.
- **Ctrl+C is safe.** If a pane is idle, sending "Ctrl c" is a no-op.
- **Clean up sessions when done** with `delete` to avoid orphaned processes
  and stale resurrection state.

## Running Inside zellij

If Crush itself runs inside zellij, `$ZELLIJ` and `$ZELLIJ_SESSION_NAME` are
set and bare `zellij action` calls target the current session. The scripts
always pass `--session` explicitly, so they work identically inside and
outside — just use session names.
