# zellij Key Names

Use these as `--keys` arguments to `zellij_send.sh` or directly in
`zellij action send-keys`. Zellij key syntax differs from tmux: modifiers
are full words separated by spaces, and some names differ (notably `Esc`,
not `Escape`).

## Control Keys

| Key Name | Meaning |
|----------|---------|
| `Ctrl c` | Ctrl+C (interrupt) |
| `Ctrl d` | Ctrl+D (EOF) |
| `Ctrl z` | Ctrl+Z (suspend) |
| `Ctrl a` | Ctrl+A |
| `Ctrl e` | Ctrl+E |
| `Ctrl k` | Ctrl+K |
| `Ctrl l` | Ctrl+L (clear) |
| `Ctrl r` | Ctrl+R (reverse search) |
| `Ctrl u` | Ctrl+U (kill line) |
| `Ctrl w` | Ctrl+W (delete word) |

Any letter works: `Ctrl b`, `Ctrl x`, ... Combos stack:
`Ctrl Shift a`, `Ctrl Alt x`.

## Special Keys

| Key Name | Meaning |
|----------|---------|
| `Enter` | Return / Enter |
| `Esc` | Escape key (NOT `Escape`) |
| `Tab` | Tab key |
| `Shift Tab` | Backtab (there is no `Backtab` key name) |
| `Space` | Space bar |
| `Backspace` | Backspace |
| `Delete` | Delete (forward) |
| `Home` / `End` | Home / End |
| `PageUp` / `PageDown` | Page navigation |
| `Insert` | Insert key |

## Arrow Keys

`Up`, `Down`, `Left`, `Right`.

## Alt/Meta Keys

Prefix with `Alt`:

| Key Name | Meaning |
|----------|---------|
| `Alt b` | Alt+B (backward word) |
| `Alt f` | Alt+F (forward word) |
| `Alt d` | Alt+D (delete word forward) |
| `Alt x` | Alt+X |
| `Alt Shift b` | Alt+Shift+B |

## Function Keys

`F1` through `F12`.

## Printable Characters

Plain characters are themselves: `a`, `b`, `/`, `:`, ... To send a key
sequence like `:w Enter` in vi, pass them as separate tokens.

## Multi-Key Sequences

Pass multiple keys in one call as separate arguments:

```bash
# Ctrl+A then 'k' (kills line in screen/readline)
zellij_send.sh -k mywork "Ctrl a" k

# Ctrl+R then a search term
zellij_send.sh -k mywork "Ctrl r" "docker"

# Enter vi command mode and save
zellij_send.sh -k mywork Esc ":w" Enter
```

## Raw Bytes

For byte-level control (or key combinations zellij cannot parse), use
`--bytes`, which maps to `zellij action write`:

```bash
zellij_send.sh -b mywork 3        # Ctrl+C
zellij_send.sh -b mywork 4        # Ctrl+D (EOF)
zellij_send.sh -b mywork 27       # raw ESC byte
zellij_send.sh -b mywork 13       # carriage return (Enter)
```

## Notes

- `write-chars` (the default send mode) never interprets key names; the
  text is written literally. Use `-k` for key names.
- A literal newline inside `write-chars` text acts as Enter. Use
  `--paste` to send multiline text without executing it.
