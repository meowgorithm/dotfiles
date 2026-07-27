---
name: bubble-tea-v2
description: Upgrade Go TUI codebases from Charm v1 to v2 libraries (Bubble Tea v2, Lip Gloss v2, Bubbles v2). Use when the user asks to migrate, upgrade, or update a Charm TUI project to v2, when code uses old import paths (github.com/charmbracelet/bubbletea, lipgloss, or bubbles) and needs the new charm.land/v2 paths, when fixing compile errors from the v2 API changes (tea.View, KeyPressMsg, MouseClickMsg, color.Color, removed Renderer, AdaptiveColor, DefaultKeyMap, etc.), or when writing new Charm v2 code and needing the current idiomatic APIs.
---

# Bubble Tea v2 Upgrade

Migrate Charm TUI projects from v1 to v2. The three libraries must be upgraded
together: Bubble Tea v2, Lip Gloss v2, and Bubbles v2.

## Workflow

1. **Update dependencies** in `go.mod`:

   ```sh
   go get charm.land/bubbletea/v2@latest
   go get charm.land/bubbles/v2@latest
   go get charm.land/lipgloss/v2@latest
   ```

2. **Rewrite import paths** across all `.go` files:

   | v1 | v2 |
   |---|---|
   | `github.com/charmbracelet/bubbletea` | `charm.land/bubbletea/v2` |
   | `github.com/charmbracelet/lipgloss` | `charm.land/lipgloss/v2` |
   | `github.com/charmbracelet/bubbles/...` | `charm.land/bubbles/v2/...` |

   Note: `bubbles/runeutil` and `bubbles/memoization` are now internal and
   cannot be imported.

3. **Apply breaking changes** by library. Read the relevant reference for the
   full old-to-new tables and examples:

   - Bubble Tea (View, keys, mouse, program options/commands/methods):
     see [references/bubbletea.md](references/bubbletea.md)
   - Lip Gloss (colors, renderer removal, printing, adaptive colors):
     see [references/lipgloss.md](references/lipgloss.md)
   - Bubbles (per-component changes):
     see [references/bubbles.md](references/bubbles.md)

4. **Build and fix iteratively**:

   ```sh
   go build ./...
   ```

   Resolve remaining compile errors using the reference tables. Re-run until
   clean, then run the project's tests.

## Highest-Impact Changes

These appear in nearly every project; address them first:

- `View() string` → `View() tea.View` (return `tea.NewView(s)`).
- `case tea.KeyMsg:` → `case tea.KeyPressMsg:`; `case " ":` → `case "space":`.
- Program options and toggle commands → declarative `tea.View` fields
  (e.g. `v.AltScreen = true`, `v.MouseMode = tea.MouseModeCellMotion`).
- `lipgloss.Color` is now a function returning `color.Color`;
  `lipgloss.TerminalColor` → `image/color.Color`.
- `lipgloss.AdaptiveColor` → `compat.AdaptiveColor` or `lipgloss.LightDark`.
- Renderer removed: `renderer.NewStyle()` / `DefaultRenderer().NewStyle()` →
  `lipgloss.NewStyle()`.
- Bubbles: `DefaultKeyMap` var → `DefaultKeyMap()` func; exported
  `Width`/`Height` fields → `SetWidth()`/`Width()` methods; `NewModel` → `New`.

## New Bubble Tea v2 Program

```go
package main

import (
	"fmt"
	"os"

	tea "charm.land/bubbletea/v2"
)

type model struct{ count int }

func (m model) Init() tea.Cmd { return nil }

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyPressMsg:
		switch msg.String() {
		case "q", "ctrl+c":
			return m, tea.Quit
		case "space":
			m.count++
		}
	case tea.MouseClickMsg:
		if msg.Button == tea.MouseLeft {
			m.count++
		}
	}
	return m, nil
}

func (m model) View() tea.View {
	v := tea.NewView(fmt.Sprintf("Count: %d\n", m.count))
	v.AltScreen = true
	v.MouseMode = tea.MouseModeCellMotion
	return v
}

func main() {
	p := tea.NewProgram(model{})
	if _, err := p.Run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
```

## Light/Dark Detection in v2

Background is no longer auto-detected. Request it in `Init` and react in
`Update`:

```go
func (m model) Init() tea.Cmd { return tea.RequestBackgroundColor }

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	if bg, ok := msg.(tea.BackgroundColorMsg); ok {
		lightDark := lipgloss.LightDark(bg.IsDark())
		m.fg = lightDark(lipgloss.Color("#333"), lipgloss.Color("#f1f1f1"))
	}
	return m, nil
}
```

For non-Bubble-Tea output, use `lipgloss.HasDarkBackground(os.Stdin, os.Stdout)`
and print via `lipgloss.Println` for automatic color downsampling.
