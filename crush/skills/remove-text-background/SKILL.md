---
name: remove-text-background
description: Remove a solid background color from an image and make it transparent, preserving anti-aliased text edges with no dark halo. Use when the user asks to "remove the background", "make the background transparent", or "delete the background color" from screenshots of text, terminals, code blocks, chat logs, markdown, or diagrams with a uniform background.
---

# Remove Text Background

Remove a uniform background color from an image (typically a screenshot of
text on a dark or light solid background) and save it as a transparent image.

Uses exact alpha uncompositing (GIMP-style "Color to Alpha") instead of
fuzz-based color keying, so anti-aliased text keeps its true color with
fractional alpha and no halo fringe. This is exact when the image was rendered
over the solid background, which is the case for UI/terminal screenshots.

## Quick Start

Background color is auto-detected from the four corner pixels:

```bash
scripts/remove_bg.py input.png output.png
```

## Options

| Option     | Description                                                  | Example         |
| ---------- | ------------------------------------------------------------ | --------------- |
| `--bg`     | Explicit background color `R,G,B` or `#hex`                  | `--bg 45,44,55` |
| `--corner` | Sample background from pixel `x,y` instead of all 4 corners  | `--corner 10,10`|

Use `--bg` when the corners are not pure background (e.g. a border or UI
chrome reaches the edge). Get the color first with ImageMagick:

```bash
magick input.png -format "%[pixel:p{10,10}]\n" info:
```

## Notes

- Output format must support alpha; use `.png` (or `.webp`).
- Requires Pillow and NumPy (`python3 -m pip install --user pillow numpy`);
  the script exits with the install command if missing.
- Verify afterwards: corner pixels should read alpha 0, e.g.
  `python3 -c "from PIL import Image; print(Image.open('out.png').getpixel((0,0)))"`.
- Do NOT use ImageMagick `-fuzz -transparent` for text screenshots; it leaves
  dark halos on anti-aliased glyphs. This skill exists to avoid that.
