# tmux Key Names and Escape Sequences

Use these as `--keys` arguments to `tmux_send.sh` or directly in
`tmux send-keys`.

## Control Keys

| Key Name | Meaning |
|-----------|---------|
| `C-c` | Ctrl+C (interrupt) |
| `C-d` | Ctrl+D (EOF) |
| `C-z` | Ctrl+Z (suspend) |
| `C-a` | Ctrl+A |
| `C-b` | Ctrl+B |
| `C-e` | Ctrl+E |
| `C-k` | Ctrl+K |
| `C-l` | Ctrl+L (clear) |
| `C-r` | Ctrl+R (reverse search) |
| `C-s` | Ctrl+S |
| `C-u` | Ctrl+U (kill line) |
| `C-w` | Ctrl+W (delete word) |
| `C-\` | Ctrl+backslash |
| `C-]` | Ctrl+] |
| `C-^` | Ctrl+^ |
| `C-_` | Ctrl+_ |
| `C-Space` | Ctrl+Space |

## Special Keys

| Key Name | Meaning |
|-----------|---------|
| `Enter` | Return / Enter |
| `Escape` | Escape key |
| `Tab` | Tab key |
| `Space` | Space bar |
| `BSpace` | Backspace |
| `DC` | Delete |
| `Home` | Home |
| `End` | End |
| `PageUp` / `PageDown` | Page navigation |
| `Insert` | Insert key |

## Arrow Keys

| Key Name | Meaning |
|-----------|---------|
| `Up` | Up arrow |
| `Down` | Down arrow |
| `Left` | Left arrow |
| `Right` | Right arrow |

## Meta/Alt Keys

Prefix with `M-`:
| `M-b` | Alt+B (backward word) |
| `M-f` | Alt+F (forward word) |
| `M-d` | Alt+D (delete word forward) |
| `M-BSpace` | Alt+Backspace (delete word backward) |
| `M-x` | Alt+X |
| `M-.` | Alt+. (yank last arg) |

## Function Keys

`F1` through `F12`.

## Multi-Key Sequences

Send multiple keys in one call by passing them as separate tokens:

```bash
# Send Ctrl+A then 'k' (kills line in screen/readline)
tmux_send.sh -k mysession:0.1 C-a k

# Send Ctrl+R then search term
tmux_send.sh -k mysession:0.1 C-r "docker"

# Enter vi command mode and save
tmux_send.sh -k mysession:0.2 Escape ":w" Enter
```

## Escape Sequence for `tmux send-keys` Without -l

When not using `-l` (literal), tmux interprets key names. To send a
literal `Enter` or `C-c` as text, use `-l`:

```bash
# This sends the Enter key:
tmux send-keys -t target "echo hello" Enter

# This sends the literal text "Enter":
tmux send-keys -t target -l "Enter"
```
