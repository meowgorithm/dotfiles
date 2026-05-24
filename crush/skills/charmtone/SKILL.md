---
name: charmtone
description: Use charmtone colors via the `charmtone` CLI or Go API (`github.com/charmbracelet/x/exp/charmtone`). Triggers when the user asks to use charmtone colors, generate color palettes, get hex values, create CSS/SCSS/Vim color variables, or work with the CharmTone palette in Go code. Also triggers on mentions of specific charmtone color names (Cherry, Charple, Julep, Hazy, etc.) or requests to pick/choose/apply charmtone colors.
---

# CharmTone

Use CharmTone colors via the CLI or Go API.

## CLI

The `charmtone` CLI renders the full palette and generates color variables.

```bash
# Show full palette in terminal (with color blocks)
charmtone

# Generate CSS custom properties
charmtone css > colors.css
# Output: --charmtone-cherry: #FF388B; --charmtone-charple: #6B50FF; ...

# Generate SCSS variables
charmtone scss > colors.scss
# Output: $cherry: #FF388B; $charple: #6B50FF; ...

# Generate Vim let variables
charmtone vim > charmtone.vim
# Output: let cherry = '#FF388B' | let charple = '#6B50FF' | ...
```

The CLI is ideal for generating static color files or quickly looking up hex values. It does not accept color name arguments — it always outputs the full palette.

## Go API

Import: `github.com/charmbracelet/x/exp/charmtone`

Use the Go API for programmatic color usage in Go programs.

```go
import "github.com/charmbracelet/x/exp/charmtone"

// Color constants
c := charmtone.Cherry
fmt.Println(c.Hex())    // "#FF388B"
fmt.Println(c.String()) // "Cherry"
r, g, b, a := c.RGBA()  // 16-bit RGBA

// Key satisfies color.Color
var _ color.Color = charmtone.Charple

// Palette groups
charmtone.Spectrum()   // Cumin through Zest (47 colors)
charmtone.Neutrals()   // Pepper through Soda (12 colors)
charmtone.Charples()   // Jelly → Darple → Charple → Larple → Hazy (5)
charmtone.Additions()  // Spinach → Gator → Pickle → Julep (4)
charmtone.Deletions()  // Toast → Steak → Pom → Cherry (4)
charmtone.Keys()       // All colors (~72)

// Type checks on Key
charmtone.Julep.IsPrimary()    // true
charmtone.Coral.IsSecondary()  // true
charmtone.Pepper.IsNeutral()   // true
charmtone.Darple.IsCharple()   // true
charmtone.Chili.IsSpectrum()   // true
```

For the complete color list, method signatures, and grouping details, see [references/go-api.md](references/go-api.md).

## Quick Reference — Key Colors

| Color | Hex | Group |
|-------|-----|-------|
| Cherry | `#FF388B` | Spectrum |
| Charple | `#6B50FF` | Spectrum / Primary |
| Julep | `#00FFB2` | Spectrum / Primary / Additions |
| Hazy | `#8B75FF` | Spectrum / Primary / Charples |
| Blush | `#FF84FF` | Spectrum / Primary |
| Dolly | `#FF60FF` | Spectrum / Primary |
| Bok | `#68FFD6` | Spectrum / Primary |
| Zest | `#E8FE96` | Spectrum / Primary |
| Butter | `#FFFAF1` | Primary |
| Ice | `#00FFFC` | Secondary |
| Pepper | `#201F26` | Neutral (dark) |
| Soda | `#FBFBFB` | Neutral (light) |

Primary colors are marked with ● in the CLI output. Secondary (○) include: Uni, Coral, Tuna, Violet, Malibu, Turtle, Ice.
