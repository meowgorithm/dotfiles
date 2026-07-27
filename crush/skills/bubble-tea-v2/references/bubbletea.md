# Bubble Tea v2 Reference

Full old-to-new migration tables for Bubble Tea v2.

## Table of Contents

- [The Big Idea: Declarative Views](#the-big-idea-declarative-views)
- [View Returns tea.View](#view-returns-teaview)
- [View Fields](#view-fields)
- [Key Messages](#key-messages)
- [Paste Messages](#paste-messages)
- [Mouse Messages](#mouse-messages)
- [Removed Program Options](#removed-program-options)
- [Removed Commands](#removed-commands)
- [Removed Program Methods](#removed-program-methods)
- [Renamed APIs](#renamed-apis)
- [New Program Options](#new-program-options)

## The Big Idea: Declarative Views

v2 replaces imperative commands/options with declarative `tea.View` fields set
in `View()`.

```go
// v1: imperative
p := tea.NewProgram(model{}, tea.WithAltScreen(), tea.WithMouseCellMotion())

// v2: declarative
func (m model) View() tea.View {
    v := tea.NewView("Hello!")
    v.AltScreen = true
    v.MouseMode = tea.MouseModeCellMotion
    return v
}
```

## View Returns tea.View

```go
// v1
func (m model) View() string { return "Hello" }

// v2
func (m model) View() tea.View { return tea.NewView("Hello") }

// v2 longer form
func (m model) View() tea.View {
    var v tea.View
    v.SetContent("Hello")
    v.AltScreen = true
    return v
}
```

## View Fields

| Field | Purpose |
|---|---|
| `Content` | Rendered string (via `SetContent()` / `NewView()`) |
| `AltScreen` | Alternate screen buffer |
| `MouseMode` | `MouseModeNone`, `MouseModeCellMotion`, `MouseModeAllMotion` |
| `ReportFocus` | Focus/blur event reporting |
| `DisableBracketedPasteMode` | Disable bracketed paste |
| `WindowTitle` | Terminal window title |
| `Cursor` | Cursor position, shape, color, blink |
| `ForegroundColor` | Terminal foreground color |
| `BackgroundColor` | Terminal background color |
| `ProgressBar` | Native terminal progress bar |
| `KeyboardEnhancements` | Keyboard enhancement features |
| `OnMouse` | Intercept mouse messages based on view content |

## Key Messages

### tea.KeyMsg is now an interface

Use `tea.KeyPressMsg` for presses (most code). Use `tea.KeyMsg` + type switch
for both presses and releases.

```go
// v1
case tea.KeyMsg:
    switch msg.String() {
    case "q":
        return m, tea.Quit
    }

// v2
case tea.KeyPressMsg:
    switch msg.String() {
    case "q":
        return m, tea.Quit
    }

// v2 handling both press and release
case tea.KeyMsg:
    switch key := msg.(type) {
    case tea.KeyPressMsg:
    case tea.KeyReleaseMsg:
    }
```

### Key field changes

| v1 | v2 | Notes |
|---|---|---|
| `msg.Type` | `msg.Code` | A `rune`: `tea.KeyEnter`, `'a'`, etc. |
| `msg.Runes` | `msg.Text` | Now `string`, not `[]rune` |
| `msg.Alt` | `msg.Mod` | `msg.Mod.Contains(tea.ModAlt)` |
| `tea.KeyRune` | — | Check `len(msg.Text) > 0` |
| `tea.KeyCtrlC` | — | `msg.String() == "ctrl+c"` or `msg.Code == 'c' && msg.Mod == tea.ModCtrl` |

### Space bar

`msg.String()` returns `"space"` instead of `" "`. `key.Code` is still `' '`
and `key.Text` is still `" "`.

```go
// v1
case " ":
// v2
case "space":
```

### Ctrl+key matching

```go
// v2 option A: string matching
case tea.KeyPressMsg:
    switch msg.String() {
    case "ctrl+c":
    }

// v2 option B: field matching
case tea.KeyPressMsg:
    if msg.Code == 'c' && msg.Mod == tea.ModCtrl {
    }
```

### New key fields (v2 only)

- `key.ShiftedCode` — shifted key code (e.g. `'B'` for shift+b)
- `key.BaseCode` — key on US PC-101 layout (international keyboards)
- `key.IsRepeat` — auto-repeating (Kitty protocol / Windows Console only)
- `key.Keystroke()` — like `String()` but always includes modifier info

## Paste Messages

Paste events are now their own message types, not `tea.KeyMsg` with a flag.

```go
// v1
case tea.KeyMsg:
    if msg.Paste {
        m.text += string(msg.Runes)
    }

// v2
case tea.PasteMsg:
    m.text += msg.Content
case tea.PasteStartMsg:
case tea.PasteEndMsg:
```

## Mouse Messages

### tea.MouseMsg is now an interface

Get coordinates via `msg.Mouse()`.

```go
// v1
case tea.MouseMsg:
    x, y := msg.X, msg.Y

// v2
case tea.MouseMsg:
    mouse := msg.Mouse()
    x, y := mouse.X, mouse.Y
```

### Events split by type

```go
// v1
case tea.MouseMsg:
    if msg.Action == tea.MouseActionPress && msg.Button == tea.MouseButtonLeft {
    }

// v2
case tea.MouseClickMsg:
    if msg.Button == tea.MouseLeft {
    }
case tea.MouseReleaseMsg:
case tea.MouseWheelMsg:
case tea.MouseMotionMsg:
```

### Button constants renamed

| v1 | v2 |
|---|---|
| `tea.MouseButtonLeft` | `tea.MouseLeft` |
| `tea.MouseButtonRight` | `tea.MouseRight` |
| `tea.MouseButtonMiddle` | `tea.MouseMiddle` |
| `tea.MouseButtonWheelUp` | `tea.MouseWheelUp` |
| `tea.MouseButtonWheelDown` | `tea.MouseWheelDown` |
| `tea.MouseButtonWheelLeft` | `tea.MouseWheelLeft` |
| `tea.MouseButtonWheelRight` | `tea.MouseWheelRight` |

`tea.MouseEvent` struct is gone; the new `tea.Mouse` struct has `X`, `Y`,
`Button`, `Mod`.

## Removed Program Options

All moved to View fields.

| Removed Option | Replacement |
|---|---|
| `tea.WithAltScreen()` | `view.AltScreen = true` |
| `tea.WithMouseCellMotion()` | `view.MouseMode = tea.MouseModeCellMotion` |
| `tea.WithMouseAllMotion()` | `view.MouseMode = tea.MouseModeAllMotion` |
| `tea.WithReportFocus()` | `view.ReportFocus = true` |
| `tea.WithoutBracketedPaste()` | `view.DisableBracketedPasteMode = true` |
| `tea.WithInputTTY()` | Remove — v2 opens TTY automatically |
| `tea.WithANSICompressor()` | Remove — new renderer optimizes automatically |

## Removed Commands

Set the corresponding View field instead.

| Removed Command | Replacement |
|---|---|
| `tea.EnterAltScreen` | `view.AltScreen = true` |
| `tea.ExitAltScreen` | `view.AltScreen = false` |
| `tea.EnableMouseCellMotion` | `view.MouseMode = tea.MouseModeCellMotion` |
| `tea.EnableMouseAllMotion` | `view.MouseMode = tea.MouseModeAllMotion` |
| `tea.DisableMouse` | `view.MouseMode = tea.MouseModeNone` |
| `tea.HideCursor` | `view.Cursor = nil` |
| `tea.ShowCursor` | `view.Cursor = &tea.Cursor{...}` or `tea.NewCursor(x, y)` |
| `tea.EnableBracketedPaste` | `view.DisableBracketedPasteMode = false` |
| `tea.DisableBracketedPaste` | `view.DisableBracketedPasteMode = true` |
| `tea.EnableReportFocus` | `view.ReportFocus = true` |
| `tea.DisableReportFocus` | `view.ReportFocus = false` |
| `tea.SetWindowTitle("...")` | `view.WindowTitle = "..."` |

## Removed Program Methods

| Removed Method | Replacement |
|---|---|
| `p.Start()` | `p.Run()` |
| `p.StartReturningModel()` | `p.Run()` |
| `p.EnterAltScreen()` | `view.AltScreen = true` in `View()` |
| `p.ExitAltScreen()` | `view.AltScreen = false` in `View()` |
| `p.EnableMouseCellMotion()` | `view.MouseMode` in `View()` |
| `p.DisableMouseCellMotion()` | `view.MouseMode = tea.MouseModeNone` in `View()` |
| `p.EnableMouseAllMotion()` | `view.MouseMode` in `View()` |
| `p.DisableMouseAllMotion()` | `view.MouseMode = tea.MouseModeNone` in `View()` |
| `p.SetWindowTitle(...)` | `view.WindowTitle` in `View()` |

## Renamed APIs

| v1 | v2 | Notes |
|---|---|---|
| `tea.Sequentially(...)` | `tea.Sequence(...)` | Was deprecated in v1 |
| `tea.WindowSize()` | `tea.RequestWindowSize` | Returns `Msg` directly, not a `Cmd` |

## New Program Options

| Option | Purpose |
|---|---|
| `tea.WithColorProfile(p)` | Force a color profile (great for testing) |
| `tea.WithWindowSize(w, h)` | Set initial terminal size (great for testing) |
