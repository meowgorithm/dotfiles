# Lip Gloss v2 Reference

Full migration tables for Lip Gloss v2.

## Table of Contents

- [Color System](#color-system)
- [Renderer Removal](#renderer-removal)
- [Printing and Color Downsampling](#printing-and-color-downsampling)
- [Background Detection and Adaptive Colors](#background-detection-and-adaptive-colors)
- [Whitespace Options](#whitespace-options)
- [Underline](#underline)
- [Style API Changes](#style-api-changes)
- [Tree Subpackage](#tree-subpackage)
- [Removed APIs](#removed-apis)
- [Quick Reference](#quick-reference)

## Color System

### Color is now a function, not a type

```go
// v1 — Color is a string type
var c lipgloss.Color = "21"

// v2 — Color is a function returning color.Color
var c color.Color = lipgloss.Color("21")
var c color.Color = lipgloss.Color("#ff00ff")
```

Return type is `image/color.Color` (stdlib).

### TerminalColor interface removed

All methods that accepted `lipgloss.TerminalColor` now accept
`image/color.Color`. Replace every `lipgloss.TerminalColor` with `color.Color`
and add `import "image/color"`.

```go
// v1
func (s Style) Foreground(c TerminalColor) Style

// v2
func (s Style) Foreground(c color.Color) Style
```

### ANSIColor is now an alias

```go
// v1
type ANSIColor uint
// v2
type ANSIColor = ansi.IndexedColor
```

v2 exports named constants for the 16 basic ANSI colors:
`lipgloss.Black`, `Red`, `Green`, `Yellow`, `Blue`, `Magenta`, `Cyan`, `White`,
and the `Bright*` variants.

### AdaptiveColor, CompleteColor, CompleteAdaptiveColor

Moved out of the root package. Use `compat` for drop-in, or `LightDark` /
`Complete` helpers for explicit control.

```go
// v1
color := lipgloss.AdaptiveColor{Light: "#0000ff", Dark: "#000099"}

// v2 — compat (quick path)
color := compat.AdaptiveColor{
    Light: lipgloss.Color("#0000ff"),
    Dark:  lipgloss.Color("#000099"),
}

// v2 — LightDark (recommended)
hasDark := lipgloss.HasDarkBackground(os.Stdin, os.Stdout)
lightDark := lipgloss.LightDark(hasDark)
color := lightDark(lipgloss.Color("#0000ff"), lipgloss.Color("#000099"))
```

```go
// v1
color := lipgloss.CompleteColor{TrueColor: "#ff00ff", ANSI256: "200", ANSI: "5"}

// v2 — compat
color := compat.CompleteColor{
    TrueColor: lipgloss.Color("#ff00ff"),
    ANSI256:   lipgloss.Color("200"),
    ANSI:      lipgloss.Color("5"),
}

// v2 — Complete (recommended)
profile := colorprofile.Detect(os.Stdout, os.Environ())
complete := lipgloss.Complete(profile)
color := complete(lipgloss.Color("5"), lipgloss.Color("200"), lipgloss.Color("#ff00ff"))
```

`compat.AdaptiveColor` fields take `color.Color` values, not strings.

## Renderer Removal

The `Renderer` type and all associated functions are removed. In v2, `Style` is
a plain value type with no renderer. Downsampling happens at the output layer.

```go
// v1 — gone
lipgloss.DefaultRenderer()
lipgloss.SetDefaultRenderer(r)
lipgloss.NewRenderer(w, opts...)
lipgloss.ColorProfile()
lipgloss.SetColorProfile(p)
renderer.NewStyle()
```

Migration:
- `lipgloss.DefaultRenderer().NewStyle()` → `lipgloss.NewStyle()`
- `renderer.NewStyle()` → `lipgloss.NewStyle()`
- Remove `*Renderer` fields from your types.
- Remove `SetColorProfile` calls; use `colorprofile.Detect` at output layer.

## Printing and Color Downsampling

In v1, downsampling happened in `Style.Render()`. In v2, `Render()` always
emits full-fidelity ANSI; downsampling happens when you print.

```go
s := someStyle.Render("Hello!")

lipgloss.Println(s)                 // stdout, auto-downsampled
lipgloss.Fprintln(os.Stderr, s)     // stderr
str := lipgloss.Sprint(s)           // string, downsampled for stdout profile
```

Customize the default writer:

```go
lipgloss.Writer = colorprofile.NewWriter(os.Stderr, os.Environ())
```

With Bubble Tea v2, no changes needed — it handles downsampling internally.

## Background Detection and Adaptive Colors

### Standalone

```go
// v1
hasDark := lipgloss.HasDarkBackground()

// v2 — specify input and output
hasDark := lipgloss.HasDarkBackground(os.Stdin, os.Stdout)

lightDark := lipgloss.LightDark(hasDark)
fg := lightDark(lipgloss.Color("#333333"), lipgloss.Color("#f1f1f1"))
s := lipgloss.NewStyle().Foreground(fg)
```

### With Bubble Tea

```go
func (m model) Init() tea.Cmd { return tea.RequestBackgroundColor }

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
    switch msg := msg.(type) {
    case tea.BackgroundColorMsg:
        m.styles = newStyles(msg.IsDark())
    }
    return m, nil
}

func newStyles(bgIsDark bool) styles {
    lightDark := lipgloss.LightDark(bgIsDark)
    return styles{
        title: lipgloss.NewStyle().Foreground(lightDark(
            lipgloss.Color("#333333"),
            lipgloss.Color("#f1f1f1"),
        )),
    }
}
```

## Whitespace Options

Separate fg/bg whitespace options replaced by a single style option.

```go
// v1
lipgloss.Place(w, h, hPos, vPos, str,
    lipgloss.WithWhitespaceForeground(lipgloss.Color("#333")),
    lipgloss.WithWhitespaceBackground(lipgloss.Color("#000")),
)

// v2
lipgloss.Place(w, h, hPos, vPos, str,
    lipgloss.WithWhitespaceStyle(lipgloss.NewStyle().
        Foreground(lipgloss.Color("#333")).
        Background(lipgloss.Color("#000")),
    ),
)
```

## Underline

`Underline(bool)` still works. v2 adds fine-grained control.

```go
s := lipgloss.NewStyle().Underline(true)  // still works

// new: specific styles
s := lipgloss.NewStyle().UnderlineStyle(lipgloss.UnderlineCurly)

// new: colored underlines
s := lipgloss.NewStyle().
    UnderlineStyle(lipgloss.UnderlineSingle).
    UnderlineColor(lipgloss.Color("#FF0000"))
```

`Underline(true)` == `UnderlineStyle(UnderlineSingle)`;
`Underline(false)` == `UnderlineStyle(UnderlineNone)`.

## Style API Changes

`NewStyle()` is no longer tied to a Renderer; it is a pure value.

Color getters return `color.Color`:

```go
// v1
fg := s.GetForeground() // TerminalColor
// v2
fg := s.GetForeground() // color.Color
```

New style methods (each has corresponding `Get*` / `Unset*`):

| Method | Description |
|---|---|
| `UnderlineStyle(Underline)` | Underline style (single, double, curly, etc.) |
| `UnderlineColor(color.Color)` | Underline color |
| `PaddingChar(rune)` | Character for padding fill |
| `MarginChar(rune)` | Character for margin fill |
| `Hyperlink(link, params...)` | Clickable hyperlink |
| `BorderForegroundBlend(...color.Color)` | Gradient border colors |
| `BorderForegroundBlendOffset(int)` | Border gradient offset |

## Tree Subpackage

```go
// v1
import "github.com/charmbracelet/lipgloss/tree"
// v2
import "charm.land/lipgloss/v2/tree"
```

New methods:
- `IndenterStyle(lipgloss.Style)` — static style for indentation
- `IndenterStyleFunc(func(Children, int) lipgloss.Style)` — conditional indent style
- `Width(int)` — tree width for padding

## Removed APIs

| v1 Symbol | v2 Replacement |
|---|---|
| `type Renderer` | Removed entirely |
| `DefaultRenderer()` | Not needed |
| `SetDefaultRenderer(r)` | Not needed |
| `NewRenderer(w, opts...)` | Not needed |
| `ColorProfile()` | `colorprofile.Detect(w, env)` |
| `SetColorProfile(p)` | Set `lipgloss.Writer.Profile` |
| `HasDarkBackground()` (no args) | `lipgloss.HasDarkBackground(in, out)` |
| `SetHasDarkBackground(b)` | Not needed — pass bool to `LightDark` |
| `type TerminalColor` | `image/color.Color` |
| `type Color string` | `func Color(string) color.Color` |
| `type ANSIColor uint` | `type ANSIColor = ansi.IndexedColor` |
| `type AdaptiveColor` | `compat.AdaptiveColor` or `LightDark` |
| `type CompleteColor` | `compat.CompleteColor` or `Complete` |
| `type CompleteAdaptiveColor` | `compat.CompleteAdaptiveColor` |
| `WithWhitespaceForeground(c)` | `WithWhitespaceStyle(s)` |
| `WithWhitespaceBackground(c)` | `WithWhitespaceStyle(s)` |
| `renderer.NewStyle()` | `lipgloss.NewStyle()` |

## Quick Reference

| Task | v1 | v2 |
|---|---|---|
| Import | `"github.com/charmbracelet/lipgloss"` | `"charm.land/lipgloss/v2"` |
| Create style | `lipgloss.NewStyle()` | `lipgloss.NewStyle()` |
| Hex color | `lipgloss.Color("#ff00ff")` | `lipgloss.Color("#ff00ff")` |
| ANSI color | `lipgloss.Color("5")` | `lipgloss.Color("5")` or `lipgloss.Magenta` |
| Adaptive color | `lipgloss.AdaptiveColor{Light: "#fff", Dark: "#000"}` | `compat.AdaptiveColor{Light: lipgloss.Color("#fff"), Dark: lipgloss.Color("#000")}` |
| Set foreground | `s.Foreground(lipgloss.Color("5"))` | `s.Foreground(lipgloss.Color("5"))` |
| Print downsampled | `fmt.Println(s.Render("hi"))` | `lipgloss.Println(s.Render("hi"))` |
| Detect dark bg | `lipgloss.HasDarkBackground()` | `lipgloss.HasDarkBackground(os.Stdin, os.Stdout)` |
| Light/dark color | `lipgloss.AdaptiveColor{...}` | `lipgloss.LightDark(isDark)(light, dark)` |
| Whitespace styling | `WithWhitespaceForeground(c)` | `WithWhitespaceStyle(lipgloss.NewStyle().Foreground(c))` |
| Underline | `s.Underline(true)` | `s.Underline(true)` or `s.UnderlineStyle(lipgloss.UnderlineCurly)` |
