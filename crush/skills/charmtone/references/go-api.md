# CharmTone Go API Reference

Package: `github.com/charmbracelet/x/exp/charmtone`

## Type

```go
type Key int
```

`Key` satisfies `color.Color` and `fmt.Stringer`.

## Methods on Key

```go
func (k Key) RGBA() (r, g, b, a uint32)   // 16-bit RGBA, satisfies color.Color
func (k Key) String() string                // color name, e.g. "Cherry"
func (k Key) Hex() string                   // e.g. "#FF388B"
func (k Key) IsSpectrum() bool              // Cumin through Zest
func (k Key) IsNeutral() bool               // Pepper through Soda
func (k Key) IsCharple() bool               // Charple ramp member
func (k Key) IsAddition() bool              // diff-addition color
func (k Key) IsDeletion() bool              // diff-deletion color
func (k Key) IsPrimary() bool               // core palette (8 colors)
func (k Key) IsSecondary() bool             // secondary palette (7 colors)
func (k Key) IsTertiary() bool              // Deprecated, calls IsSecondary()
```

## Functions (return []Key)

```go
func Keys() []Key       // All colors (~72), in iota order
func Spectrum() []Key   // Cumin through Zest (47 colors)
func Neutrals() []Key   // Pepper through Soda (12 colors)
func Charples() []Key   // Jelly → Darple → Charple → Larple → Hazy (5)
func Additions() []Key  // Spinach → Gator → Pickle → Julep (4)
func Deletions() []Key  // Toast → Steak → Pom → Cherry (4)
```

## All Color Constants

### Main Spectrum (Cumin → Zest)

| Constant | Hex | Constant | Hex | Constant | Hex |
|----------|-----|----------|-----|----------|-----|
| `Cumin` | `#BF976F` | `Tang` | `#FF985A` | `Yam` | `#FFB587` |
| `Paprika` | `#D36C64` | `Bengal` | `#FF6E63` | `Uni` | `#FF937D` |
| `Sriracha` | `#EB4268` | `Coral` | `#FF577D` | `Salmon` | `#FF7F90` |
| `Chili` | `#E23080` | `Cherry` | `#FF388B` | `Tuna` | `#FF6DAA` |
| `Macaron` | `#E940B0` | `Pony` | `#FF4FBF` | `Cheeky` | `#FF79D0` |
| `Flamingo` | `#F947E3` | `Dolly` | `#FF60FF` | `Blush` | `#FF84FF` |
| `Urchin` | `#C337E0` | `Mochi` | `#EB5DFF` | `Lilac` | `#F379FF` |
| `Prince` | `#9C35E1` | `Violet` | `#C259FF` | `Mauve` | `#D46EFF` |
| `Grape` | `#7134DD` | `Plum` | `#9953FF` | `Orchid` | `#AD6EFF` |
| `Jelly` | `#4A30D9` | `Charple` | `#6B50FF` | `Hazy` | `#8B75FF` |
| `Ox` | `#3331B2` | `Sapphire` | `#4949FF` | `Guppy` | `#7272FF` |
| `Oceania` | `#2B55B3` | `Thunder` | `#4776FF` | `Anchovy` | `#719AFC` |
| `Damson` | `#007AB8` | `Malibu` | `#00A4FF` | `Sardine` | `#4FBEFE` |
| `Zinc` | `#10B1AE` | `Turtle` | `#0ADCD9` | `Lichen` | `#5CDFEA` |
| `Guac` | `#12C78F` | `Julep` | `#00FFB2` | `Bok` | `#68FFD6` |
| `Mustard` | `#F5EF34` | `Citron` | `#E8FF27` | `Zest` | `#E8FE96` |

### Butter

| Constant | Hex |
|----------|-----|
| `Butter` | `#FFFAF1` |

### Neutrals (Pepper → Soda)

| Constant | Hex | Constant | Hex | Constant | Hex |
|----------|-----|----------|-----|----------|-----|
| `Pepper` | `#201F26` | `BBQ` | `#2D2C36` | `Char` | `#3A3943` |
| `Iron` | `#4D4C57` | `Oyster` | `#605F6B` | `Squid` | `#858392` |
| `Steam` | `#A2A0AD` | `Smoke` | `#BFBCC8` | `Steep` | `#D6D3DC` |
| `Sash` | `#ECEBF0` | `Salt` | `#F7F6FB` | `Soda` | `#FBFBFB` |

### Charples

| Constant | Hex |
|----------|-----|
| `Darple` | `#5B40EC` |
| `Larple` | `#7B62FF` |

### Diff Additions

| Constant | Hex |
|----------|-----|
| `Pickle` | `#00A475` |
| `Gator` | `#18463D` |
| `Spinach` | `#1C3634` |

### Diff Deletions

| Constant | Hex |
|----------|-----|
| `Pom` | `#AB2454` |
| `Steak` | `#582238` |
| `Toast` | `#412130` |

### Secondary

| Constant | Hex |
|----------|-----|
| `Ice` | `#00FFFC` |

### Deprecated Aliases

```go
const Charcoal = Char  // Deprecated: renamed to Char
const Ash = Sash       // Deprecated: renamed to Sash
```

## Palette Membership Summary

| Group | Count | Colors |
|-------|-------|--------|
| **Primary** | 8 | Charple, Dolly, Julep, Zest, Hazy, Blush, Bok, Butter |
| **Secondary** | 7 | Uni, Coral, Tuna, Violet, Malibu, Turtle, Ice |
| **Spectrum** | 47 | Cumin through Zest |
| **Neutrals** | 12 | Pepper through Soda |
| **Charples** | 5 | Jelly, Darple, Charple, Larple, Hazy |
| **Additions** | 4 | Spinach, Gator, Pickle, Julep |
| **Deletions** | 4 | Toast, Steak, Pom, Cherry |
