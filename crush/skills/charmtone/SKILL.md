---
name: charmtone
description: Use charmtone colors via the `charmtone` CLI or Go API (`github.com/charmbracelet/x/exp/charmtone`). Triggers when the user asks to use charmtone colors, generate color palettes, get hex values, create CSS/SCSS/Vim color variables, or work with the CharmTone palette in Go code. Also triggers on mentions of specific charmtone color names (Cherry, Charple, Julep, Hazy, etc.) or requests to pick/choose/apply charmtone colors.
---

# CharmTone

Use CharmTone colors via the CLI or Go API. The complete palette is listed
below — refer to it directly rather than guessing hex values.

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

The CLI is ideal for generating static color files or quickly looking up hex
values. It does not accept color name arguments — it always outputs the full
palette.

## Go API

Import: `github.com/charmbracelet/x/exp/charmtone`

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

See [references/go-api.md](references/go-api.md) for full method signatures
and grouping details.

## Complete Color Palette

All 68 CharmTone colors, grouped by palette.

### Spectrum (47 colors, Cumin → Zest)

| Color | Hex | Color | Hex | Color | Hex |
|-------|-----|-------|-----|-------|-----|
| Cumin | `#BF976F` | Tang | `#FF985A` | Yam | `#FFB587` |
| Paprika | `#D36C64` | Bengal | `#FF6E63` | Uni | `#FF937D` |
| Sriracha | `#EB4268` | Coral | `#FF577D` | Salmon | `#FF7F90` |
| Chili | `#E23080` | Cherry | `#FF388B` | Tuna | `#FF6DAA` |
| Macaron | `#E940B0` | Pony | `#FF4FBF` | Cheeky | `#FF79D0` |
| Flamingo | `#F947E3` | Dolly | `#FF60FF` | Blush | `#FF84FF` |
| Urchin | `#C337E0` | Mochi | `#EB5DFF` | Lilac | `#F379FF` |
| Prince | `#9C35E1` | Violet | `#C259FF` | Mauve | `#D46EFF` |
| Grape | `#7134DD` | Plum | `#9953FF` | Orchid | `#AD6EFF` |
| Jelly | `#4A30D9` | Charple | `#6B50FF` | Hazy | `#8B75FF` |
| Ox | `#3331B2` | Sapphire | `#4949FF` | Guppy | `#7272FF` |
| Oceania | `#2B55B3` | Thunder | `#4776FF` | Anchovy | `#719AFC` |
| Damson | `#007AB8` | Malibu | `#00A4FF` | Sardine | `#4FBEFE` |
| Zinc | `#10B1AE` | Turtle | `#0ADCD9` | Lichen | `#5CDFEA` |
| Guac | `#12C78F` | Julep | `#00FFB2` | Bok | `#68FFD6` |
| Mustard | `#F5EF34` | Citron | `#E8FF27` | Zest | `#E8FE96` |

### Butter

| Color | Hex |
|-------|-----|
| Butter | `#FFFAF1` |

### Neutrals (12 colors, Pepper → Soda)

| Color | Hex | Color | Hex | Color | Hex |
|-------|-----|-------|-----|-------|-----|
| Pepper | `#201F26` | BBQ | `#2D2C36` | Char | `#3A3943` |
| Iron | `#4D4C57` | Oyster | `#605F6B` | Squid | `#858392` |
| Steam | `#A2A0AD` | Smoke | `#BFBCC8` | Steep | `#D6D3DC` |
| Sash | `#ECEBF0` | Salt | `#F7F6FB` | Soda | `#FBFBFB` |

### Charples (5 colors: Jelly → Darple → Charple → Larple → Hazy)

| Color | Hex |
|-------|-----|
| Jelly | `#4A30D9` |
| Darple | `#5B40EC` |
| Charple | `#6B50FF` |
| Larple | `#7B62FF` |
| Hazy | `#8B75FF` |

### Diff Additions (Spinach → Gator → Pickle → Julep)

| Color | Hex |
|-------|-----|
| Spinach | `#1C3634` |
| Gator | `#18463D` |
| Pickle | `#00A475` |
| Julep | `#00FFB2` |

### Diff Deletions (Toast → Steak → Pom → Cherry)

| Color | Hex |
|-------|-----|
| Toast | `#412130` |
| Steak | `#582238` |
| Pom | `#AB2454` |
| Cherry | `#FF388B` |

### Secondary extras

| Color | Hex |
|-------|-----|
| Ice | `#00FFFC` |

### Deprecated aliases

| Alias | Renamed to |
|-------|------------|
| Charcoal | Char |
| Ash | Sash |

## Palette Membership

| Group | Count | Colors |
|-------|-------|--------|
| **Primary** | 8 | Charple, Dolly, Julep, Zest, Hazy, Blush, Bok, Butter |
| **Secondary** | 7 | Uni, Coral, Tuna, Violet, Malibu, Turtle, Ice |
| **Spectrum** | 47 | Cumin through Zest |
| **Neutrals** | 12 | Pepper through Soda |
| **Charples** | 5 | Jelly, Darple, Charple, Larple, Hazy |
| **Additions** | 4 | Spinach, Gator, Pickle, Julep |
| **Deletions** | 4 | Toast, Steak, Pom, Cherry |

Primary colors are marked with ● in the CLI output, secondary with ○.
