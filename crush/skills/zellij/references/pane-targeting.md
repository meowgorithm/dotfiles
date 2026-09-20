# zellij Targeting Reference

How sessions, tabs, and panes are identified when driving zellij from
scripts.

## Sessions

Sessions are identified by exact name. All scripts pass
`zellij --session <name>` explicitly, which is required once more than one
session exists (bare `zellij action` from outside fails with "Please specify
the session name").

```bash
zellij list-sessions -s          # machine-readable names, one per line
zellij list-sessions -s -n       # same, no formatting (safe for parsing)
```

`zellij list-sessions` exits 1 when no sessions exist.

Creating detached sessions:

```bash
zellij attach --create-background --create mywork
```

- Idempotent in spirit: prints "Session already exists" and exits 1 if the
  session is already there (`zellij_session.sh new` handles this).
- The initial pane inherits the cwd of the creating process, so `cd` to the
  desired directory first.
- Headless sessions (no attached client) keep running and accept actions.

`kill-session` stops a session but keeps resurrection state;
`delete-session` removes both.

## Panes

Panes have stable, session-scoped IDs:

| Form | Example | Meaning |
|------|---------|---------|
| `terminal_N` | `terminal_3` | A terminal pane |
| `plugin_N` | `plugin_1` | A plugin pane (tab-bar, status-bar, plugins) |
| bare `N` | `3` | Shorthand for `terminal_N` |

IDs come from pane creation commands (all print the new ID):

```bash
zellij --session mywork action new-pane --direction right   # -> terminal_5
zellij --session mywork run -- bash -c 'make'               # -> terminal_6
zellij --session mywork action new-tab --name logs          # -> tab id
```

Discovery:

```bash
# Table
zellij --session mywork action list-panes
zellij --session mywork action list-panes --all

# JSON for scripting
zellij --session mywork action list-panes --json
```

Useful JSON fields per pane: `id`, `is_plugin`, `is_selectable`,
`is_focused`, `is_floating`, `exited`, `exit_status`, `title`,
`pane_command`, `pane_cwd`, `pane_rows`, `pane_columns`, `tab_id`,
`tab_name`.

Resolve the "default" pane (what the scripts do when no pane is given):

```bash
zellij --session mywork action list-panes --json | jq -r '
  [.[] | select(.is_plugin == false and .is_selectable)] as $sel
  | (([$sel[] | select(.is_focused)] | first) // ($sel | first))
  | if . == null then "" else "terminal_\(.id)" end
'
```

The `is_plugin == false` filter matters: with a client attached, the tab-bar
and status-bar show up as plugin panes and must not be targeted for text
input.

## Tabs

Tabs have a position (0-based display order) and a stable ID (0-based,
survives renames and reordering). Prefer stable IDs in scripts.

```bash
zellij --session mywork action list-tabs            # TAB_ID POSITION NAME
zellij --session mywork action query-tab-names      # names only
zellij --session mywork action new-tab --name logs  # prints new tab id
zellij --session mywork action go-to-tab 2          # by index
zellij --session mywork action go-to-tab-name logs  # by name
zellij --session mywork action rename-tab-by-id 1 api
zellij --session mywork action close-tab-by-id 1
```

Caveats:

- Tab focus is per-client. A headless session has no "current tab"
  (`current-tab-info` errors with "No active tab found for current
  client"), so `go-to-tab` only affects attached clients.
- `new-pane --tab-id <id>` places a pane in a specific tab headless.
- `list-panes` lists panes across all tabs, with `tab_id`/`tab_name` on
  each.

## Headless Geometry

With no attached client, zellij renders panes at a small default size
(50x48 for the first pane observed on 0.45, shrinking with each split).
Dumped output wraps at the pane width. Attach a client for real sizes:

```bash
zellij attach mywork
```

Check attached clients:

```bash
zellij --session mywork action list-clients
```
