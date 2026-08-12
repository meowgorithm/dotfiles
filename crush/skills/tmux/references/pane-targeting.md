# tmux Target Formats Reference

Every tmux command that accepts a `-t` flag uses these formats.

## Sessions

| Format | Example | Meaning |
|--------|---------|---------|
| `name` | `main` | Session by exact name |
| `name$` | `main$` | Exact match (no prefix expansion) |
| `=name` | `=main` | Prefix match |
| `:id` | `:3` | Session ID (`$3`) |

## Windows

| Format | Example | Meaning |
|--------|---------|---------|
| `name` | `editor` | Window by name |
| `:index` | `:2` | Window index |
| `:name` | `:editor` | Window by name |
| `:id` | `:@3` | Window ID |
| `:{start,end}` | `:0,2` | Range of windows |

## Panes

| Format | Example | Meaning |
|--------|---------|---------|
| `.index` | `.1` | Pane index in current window |
| `.%id` | `.%3` | Pane by pane ID |
| `.title` | `.editor` | Pane by title |
| `session:window.pane` | `main:1.2` | Full path to pane |
| `:window.pane` | `:1.2` | Pane in specific window |
| `session:.pane` | `main:.2` | Pane in session's current window |

## Within Current tmux (from inside a pane)

When Crush runs inside tmux, `$TMUX_PANE` holds the current pane's
target string. All targeting can be relative to the current pane:

```bash
tmux send-keys -t "$TMUX_PANE:0.1" "ls" Enter
```

But it is more reliable to discover and use explicit targets:

```bash
# Get current session and window
SESSION=$(tmux display-message -p '#{session_name}')
WINDOW=$(tmux display-message -p '#{window_index}')

# Target pane 0 in window 1
tmux send-keys -t "${SESSION}:${WINDOW}.0" "ls" Enter
```

## Wildcards in Targets

| Pattern | Matches |
|---------|---------|
| `*` | Any window or pane (not useful alone) |
| `:.*` | All panes in current window |
| `+` | Last active pane |

## Examples

```bash
# Active pane in session "work"
tmux capture-pane -t work -p

# Pane 2 in window 1 of session "work"
tmux capture-pane -t work:1.2 -p

# The pane with ID %5
tmux capture-pane -t %5 -p

# Active pane in window 0 of current session
tmux capture-pane -t :0 -p
```
