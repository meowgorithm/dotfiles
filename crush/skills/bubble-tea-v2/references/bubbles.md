# Bubbles v2 Reference

Per-component migration tables for Bubbles v2. Requires Bubble Tea v2 and
Lip Gloss v2.

## Table of Contents

- [Global Patterns](#global-patterns)
- [Cursor](#cursor)
- [Filepicker](#filepicker)
- [Help](#help)
- [List](#list)
- [Paginator](#paginator)
- [Progress](#progress)
- [Spinner](#spinner)
- [Stopwatch](#stopwatch)
- [Table](#table)
- [Textarea](#textarea)
- [Textinput](#textinput)
- [Timer](#timer)
- [Viewport](#viewport)
- [Light and Dark Styles](#light-and-dark-styles)
- [Removed Symbols Reference](#removed-symbols-reference)

## Global Patterns

### tea.KeyMsg → tea.KeyPressMsg

```go
// v1
case tea.KeyMsg:
// v2
case tea.KeyPressMsg:
```

### Exported Width/Height fields → getter/setter methods

```go
// v1
m.Width = 40
m.Height = 20
fmt.Println(m.Width, m.Height)

// v2
m.SetWidth(40)
m.SetHeight(20)
fmt.Println(m.Width(), m.Height())
```

Affected: `filepicker`, `help`, `progress`, `table`, `textinput`, `viewport`.

### DefaultKeyMap variables → functions

```go
// v1
km := textinput.DefaultKeyMap
km.Paste.SetEnabled(false)

// v2
km := textinput.DefaultKeyMap()
km.Paste.SetEnabled(false)
```

Affected: `paginator`, `textarea`, `textinput`.

### AdaptiveColor → LightDark with isDark bool

Style functions that auto-adapted now require an explicit `isDark bool`.
See [Light and Dark Styles](#light-and-dark-styles).

### Removed NewModel aliases

All `NewModel` variables removed. Use `New` directly.
Affected: `help`, `list`, `paginator`, `spinner`, `textinput`.

## Cursor

| v1 | v2 |
|----|-----|
| `model.Blink` | `model.IsBlinked` |
| `model.BlinkCmd()` | `model.Blink()` |

## Filepicker

| v1 | v2 |
|----|-----|
| `DefaultStylesWithRenderer(r)` | `DefaultStyles()` |
| `model.Height = 10` | `model.SetHeight(10)` |
| `_ = model.Height` | `_ = model.Height()` |

## Help

| v1 | v2 |
|----|-----|
| `model.Width = 80` | `model.SetWidth(80)` |
| `_ = model.Width` | `_ = model.Width()` |
| `NewModel()` | `New()` |

New functions: `DefaultStyles(isDark bool)`, `DefaultDarkStyles()`,
`DefaultLightStyles()`. Apply styles explicitly:

```go
// v1: colors auto-adapted
h := help.New()

// v2
h := help.New()
h.Styles = help.DefaultStyles(isDark)
```

## List

| v1 | v2 |
|----|-----|
| `DefaultStyles()` | `DefaultStyles(isDark)` |
| `NewDefaultItemStyles()` | `NewDefaultItemStyles(isDark)` |
| `styles.FilterPrompt` | `styles.Filter.Focused.Prompt` / `styles.Filter.Blurred.Prompt` |
| `styles.FilterCursor` | `styles.Filter.Cursor` |
| `NewModel(...)` | `New(...)` |

`Styles.FilterPrompt` and `Styles.FilterCursor` consolidated into
`Styles.Filter` (a `textinput.Styles` struct).

## Paginator

| v1 | v2 |
|----|-----|
| `DefaultKeyMap` (var) | `DefaultKeyMap()` (func) |
| `model.UsePgUpPgDownKeys` | Removed — customize `KeyMap` directly |
| `model.UseLeftRightKeys` | Removed — customize `KeyMap` directly |
| `model.UseUpDownKeys` | Removed — customize `KeyMap` directly |
| `model.UseHLKeys` | Removed — customize `KeyMap` directly |
| `model.UseJKKeys` | Removed — customize `KeyMap` directly |
| `NewModel(...)` | `New(...)` |

## Progress

Most extensive changes.

### Width

```go
// v1
p.Width = 40
// v2
p.SetWidth(40)
fmt.Println(p.Width())
```

### Colors (string → image/color.Color)

```go
// v1
p.FullColor = "#FF0000"
p.EmptyColor = "#333333"

// v2
p.FullColor = lipgloss.Color("#FF0000")
p.EmptyColor = lipgloss.Color("#333333")
```

### Gradient/Blend options

```go
// v1
progress.New(progress.WithGradient("#5A56E0", "#EE6FF8"))
progress.New(progress.WithDefaultGradient())
progress.New(progress.WithScaledGradient("#5A56E0", "#EE6FF8"))
progress.New(progress.WithDefaultScaledGradient())
progress.New(progress.WithSolidFill("#7571F9"))

// v2
progress.New(progress.WithColors(lipgloss.Color("#5A56E0"), lipgloss.Color("#EE6FF8")))
progress.New(progress.WithDefaultBlend())
progress.New(progress.WithColors(lipgloss.Color("#5A56E0"), lipgloss.Color("#EE6FF8")), progress.WithScaled(true))
progress.New(progress.WithDefaultBlend(), progress.WithScaled(true))
progress.New(progress.WithColors(lipgloss.Color("#7571F9")))
```

| v1 | v2 |
|----|-----|
| `WithGradient(a, b string)` | `WithColors(colors ...color.Color)` |
| `WithDefaultGradient()` | `WithDefaultBlend()` |
| `WithScaledGradient(a, b string)` | `WithColors(...) + WithScaled(true)` |
| `WithDefaultScaledGradient()` | `WithDefaultBlend() + WithScaled(true)` |
| `WithSolidFill(string)` | `WithColors(color)` (single color) |
| `WithColorProfile(termenv.Profile)` | Removed (automatic) |
| `Update() (tea.Model, tea.Cmd)` | `Update() (Model, tea.Cmd)` |

New: `WithColorFunc(func(total, current float64) color.Color)` for dynamic
per-cell coloring; `WithScaled(bool)`.

## Spinner

| v1 | v2 |
|----|-----|
| `NewModel()` | `New()` |
| `spinner.Tick()` (package func) | `model.Tick()` (method) |

## Stopwatch

```go
// v1
sw := stopwatch.NewWithInterval(500 * time.Millisecond)
// v2
sw := stopwatch.New(stopwatch.WithInterval(500 * time.Millisecond))
```

| v1 | v2 |
|----|-----|
| `NewWithInterval(d)` | `New(WithInterval(d))` |

## Table

| v1 | v2 |
|----|-----|
| `model.viewport.Width` | `model.Width()` / `model.SetWidth(w)` |
| `model.viewport.Height` | `model.Height()` / `model.SetHeight(h)` |

Table already had `SetWidth`/`SetHeight`/`Width()`/`Height()` in v1; internally
these now use viewport getter/setters.

## Textarea

### KeyMap

```go
// v1
km := textarea.DefaultKeyMap
// v2
km := textarea.DefaultKeyMap()
```

New key bindings: `PageUp`, `PageDown`.

### Styles

```go
// v1
ta.FocusedStyle.Base = lipgloss.NewStyle().Border(lipgloss.RoundedBorder())
ta.BlurredStyle.Base = lipgloss.NewStyle().Border(lipgloss.HiddenBorder())

// v2: styles nested under Styles struct, access via Styles.Focused / Styles.Blurred (type StyleState)
```

| v1 | v2 |
|----|-----|
| `textarea.Style` (type) | `textarea.StyleState` (type) |
| `model.FocusedStyle` | `model.Styles.Focused` |
| `model.BlurredStyle` | `model.Styles.Blurred` |
| `DefaultStyles() (focused, blurred Style)` | `DefaultStyles(isDark bool) Styles` |

### Cursor

```go
// v1
ta.Cursor              // cursor.Model (virtual cursor)
ta.SetCursor(col)      // set cursor column

// v2
ta.Cursor()            // func() *tea.Cursor (real cursor)
ta.SetCursorColumn(col)
ta.VirtualCursor       // bool: true = virtual, false = real
ta.Styles.Cursor       // CursorStyle for cursor appearance
```

New: `Column()`, `ScrollYOffset()`, `ScrollPosition()`, `MoveToBeginning()`,
`MoveToEnd()`.

## Textinput

### KeyMap

```go
// v1
km := textinput.DefaultKeyMap
// v2
km := textinput.DefaultKeyMap()
```

### Width

```go
// v1
ti.Width = 40
// v2
ti.SetWidth(40)
```

### Styles

Individual style fields moved into a `Styles` struct:

```go
// v1
ti.PromptStyle = lipgloss.NewStyle().Foreground(lipgloss.Color("63"))
ti.TextStyle = lipgloss.NewStyle()
ti.PlaceholderStyle = lipgloss.NewStyle().Foreground(lipgloss.Color("240"))
ti.CompletionStyle = lipgloss.NewStyle().Foreground(lipgloss.Color("240"))

// v2
s := textinput.DefaultStyles(isDark)
s.Focused.Prompt = lipgloss.NewStyle().Foreground(lipgloss.Color("63"))
s.Focused.Text = lipgloss.NewStyle()
s.Focused.Placeholder = lipgloss.NewStyle().Foreground(lipgloss.Color("240"))
s.Focused.Suggestion = lipgloss.NewStyle().Foreground(lipgloss.Color("240"))
ti.SetStyles(s)
```

| v1 Field | v2 Location |
|----------|-------------|
| `Model.PromptStyle` | `StyleState.Prompt` |
| `Model.TextStyle` | `StyleState.Text` |
| `Model.PlaceholderStyle` | `StyleState.Placeholder` |
| `Model.CompletionStyle` | `StyleState.Suggestion` |
| `Model.CursorStyle` | `Styles.Cursor` |
| `Model.Cursor` (cursor.Model) | `Model.Cursor()` (func → *tea.Cursor) |

New: `Model.Styles()` / `Model.SetStyles(Styles)`;
`Model.VirtualCursor()` / `Model.SetVirtualCursor(bool)`.

## Timer

```go
// v1
t := timer.NewWithInterval(30*time.Second, 100*time.Millisecond)
// v2
t := timer.New(30*time.Second, timer.WithInterval(100*time.Millisecond))
```

| v1 | v2 |
|----|-----|
| `NewWithInterval(timeout, interval)` | `New(timeout, WithInterval(interval))` |

## Viewport

### Constructor

```go
// v1
vp := viewport.New(80, 24)

// v2
vp := viewport.New(viewport.WithWidth(80), viewport.WithHeight(24))
// or
vp := viewport.New()
vp.SetWidth(80)
vp.SetHeight(24)
```

### Width, Height, YOffset

```go
// v1
vp.Width = 80
vp.Height = 24
vp.YOffset = 5

// v2
vp.SetWidth(80)
vp.SetHeight(24)
vp.SetYOffset(5)
fmt.Println(vp.Width(), vp.Height(), vp.YOffset())
```

### Removed

`HighPerformanceRendering` — removed entirely.

### New features (non-breaking)

- Soft wrapping: `vp.SoftWrap = true`
- Left gutter for line numbers via `vp.LeftGutterFunc`
- Highlighting: `SetHighlights`, `HighlightNext`, `HighlightPrevious`,
  `ClearHighlights`
- `SetContentLines([]string)`, `GetContent() string`
- `FillHeight bool` — fill with empty lines
- `StyleLineFunc func(int) lipgloss.Style` — per-line styling
- Horizontal scrolling (arrow keys, mouse wheel)

## Light and Dark Styles

Lip Gloss v2 removes `AdaptiveColor`; Bubbles no longer auto-detects the
terminal background. Choose light or dark explicitly.

### Recommended: query via Bubble Tea

```go
func (m model) Init() tea.Cmd { return tea.RequestBackgroundColor }

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
    switch msg := msg.(type) {
    case tea.BackgroundColorMsg:
        isDark := msg.IsDark()
        m.help.Styles = help.DefaultStyles(isDark)
        m.list.Styles = list.DefaultStyles(isDark)
    }
    return m, nil
}
```

Required when using Wish to detect the client's background.

### Quick: compat package

```go
import "charm.land/lipgloss/v2/compat"

var isDark = compat.HasDarkBackground()

func main() {
    h := help.New()
    h.Styles = help.DefaultStyles(isDark)
}
```

Warning: `compat` uses blocking I/O outside Bubble Tea's event loop and will not
detect remote client backgrounds over SSH.

### Manual

```go
h.Styles = help.DefaultDarkStyles()
h.Styles = help.DefaultLightStyles()
```

## Removed Symbols Reference

| Package | Removed | Replacement |
|---------|---------|-------------|
| `cursor` | `Model.Blink` | `Model.IsBlinked` |
| `cursor` | `Model.BlinkCmd()` | `Model.Blink()` |
| `filepicker` | `DefaultStylesWithRenderer(r)` | `DefaultStyles()` |
| `filepicker` | `Model.Height` (field) | `SetHeight()` / `Height()` |
| `help` | `NewModel` | `New()` |
| `help` | `Model.Width` (field) | `SetWidth()` / `Width()` |
| `list` | `NewModel` | `New()` |
| `list` | `DefaultStyles()` | `DefaultStyles(isDark)` |
| `list` | `NewDefaultItemStyles()` | `NewDefaultItemStyles(isDark)` |
| `list` | `Styles.FilterPrompt` | `Styles.Filter` (`textinput.Styles`) |
| `list` | `Styles.FilterCursor` | `Styles.Filter.Cursor` |
| `paginator` | `DefaultKeyMap` (var) | `DefaultKeyMap()` (func) |
| `paginator` | `NewModel` | `New()` |
| `paginator` | `UsePgUpPgDownKeys` etc. | Customize `KeyMap` directly |
| `progress` | `WithGradient(a, b)` | `WithColors(colors...)` |
| `progress` | `WithDefaultGradient()` | `WithDefaultBlend()` |
| `progress` | `WithScaledGradient(a, b)` | `WithColors(...) + WithScaled(true)` |
| `progress` | `WithDefaultScaledGradient()` | `WithDefaultBlend() + WithScaled(true)` |
| `progress` | `WithSolidFill(string)` | `WithColors(color)` |
| `progress` | `WithColorProfile(p)` | Removed (automatic) |
| `progress` | `Model.Width` (field) | `SetWidth()` / `Width()` |
| `spinner` | `NewModel` | `New()` |
| `spinner` | `Tick()` (package func) | `Model.Tick()` |
| `stopwatch` | `NewWithInterval(d)` | `New(WithInterval(d))` |
| `table` | `Model.Width` (field) | `SetWidth()` / `Width()` |
| `table` | `Model.Height` (field) | `SetHeight()` / `Height()` |
| `textarea` | `DefaultKeyMap` (var) | `DefaultKeyMap()` (func) |
| `textarea` | `Style` (type) | `StyleState` (type) |
| `textarea` | `Model.FocusedStyle` | `Model.Styles.Focused` |
| `textarea` | `Model.BlurredStyle` | `Model.Styles.Blurred` |
| `textarea` | `Model.SetCursor(col)` | `Model.SetCursorColumn(col)` |
| `textarea` | `DefaultStyles()` | `DefaultStyles(isDark)` |
| `textinput` | `DefaultKeyMap` (var) | `DefaultKeyMap()` (func) |
| `textinput` | `NewModel` | `New()` |
| `textinput` | `Model.Width` (field) | `SetWidth()` / `Width()` |
| `textinput` | `Model.PromptStyle` | `StyleState.Prompt` |
| `textinput` | `Model.TextStyle` | `StyleState.Text` |
| `textinput` | `Model.PlaceholderStyle` | `StyleState.Placeholder` |
| `textinput` | `Model.CompletionStyle` | `StyleState.Suggestion` |
| `textinput` | `Model.CursorStyle` | `Styles.Cursor` |
| `textinput` | `Model.Cursor` (cursor.Model) | `Model.Cursor()` (func → *tea.Cursor) |
| `timer` | `NewWithInterval(t, i)` | `New(t, WithInterval(i))` |
| `viewport` | `New(w, h int)` | `New(...Option)` |
| `viewport` | `Model.Width` (field) | `SetWidth()` / `Width()` |
| `viewport` | `Model.Height` (field) | `SetHeight()` / `Height()` |
| `viewport` | `Model.YOffset` (field) | `SetYOffset()` / `YOffset()` |
| `viewport` | `HighPerformanceRendering` | Removed |
| `runeutil` | Entire package | Moved to `internal/runeutil` (not importable) |
