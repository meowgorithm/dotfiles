---
name: border-trim-extend
description: Trim uniform borders from images and extend with the same color or a specified color. Use when asked to "trim border", "remove border and add back", "extend padding", or when uniform borders need to be trimmed then re-added with potentially different sizing.
---

# Border Trim and Extend

Trim uniform borders from images and re-add borders with the same or different color.

## Quick Start

For automatic trim-and-extend with the same border color:

```bash
~/.config/crush/skills/border-trim-extend/scripts/trim_extend.sh <input> <border-px> <output>
```

Example:
```bash
~/.config/crush/skills/border-trim-extend/scripts/trim_extend.sh image.png 75 image-bordered.png
```

## Manual Workflow

When you need more control:

**1. Detect the border color:**
```bash
magick image.png[1x1+0+0] -format "%[pixel:s]" info:
```

**2. Trim and extend:**
```bash
magick image.png -trim +repage -bordercolor "<color>" -border <px> output.png
```

## Common Use Cases

**Trim to content only (no border added):**
```bash
magick image.png -trim +repage output.png
```

**Trim and add white border:**
```bash
magick image.png -trim +repage -bordercolor white -border 50 output.png
```

**Trim and add transparent border:**
```bash
magick image.png -trim +repage -bordercolor transparent -border 100 output.png
```

## Parameters

- `-trim` - Remove uniform border pixels
- `+repage` - Reset virtual canvas after trim
- `-bordercolor` - Color for new border (hex, name, or rgba)
- `-border <px>` - Border width in pixels on all sides

## Requirements

ImageMagick (magick command) must be installed.
