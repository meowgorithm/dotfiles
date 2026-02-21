---
name: image-convert
description: Convert images between formats (PNG, JPEG, WebP, GIF, BMP, TIFF, AVIF, HEIC) with quality control and resizing. Use when the user needs to convert images, batch process multiple files, optimize image sizes, or convert to modern formats like WebP or AVIF.
---

# Image Convert

Convert images between formats with quality control, resizing, and batch
processing support.

## Supported Formats

| Format | Extension    | Notes                                |
| ------ | ------------ | ------------------------------------ |
| PNG    | .png         | Lossless, supports transparency      |
| JPEG   | .jpg, .jpeg  | Lossy compression, no transparency   |
| WebP   | .webp        | Modern format, good compression      |
| GIF    | .gif         | Animation support (first frame only) |
| BMP    | .bmp         | Uncompressed, large files            |
| TIFF   | .tiff, .tif  | High quality, large files            |
| AVIF   | .avif        | Next-gen format, best compression    |
| HEIC   | .heic, .heif | Apple format                         |

## Quick Start

### Single File Conversion

Convert by specifying output extension:

```bash
scripts/convert.sh input.png output.jpg
```

Or specify format explicitly:

```bash
scripts/convert.sh -f webp input.png output.webp
```

### With Quality and Resize

```bash
# Convert to JPEG with 90% quality and resize to 1920px width
scripts/convert.sh -f jpg -q 90 -r 1920x input.png output.jpg

# Convert to WebP at 50% of original size
scripts/convert.sh -f webp -q 85 -r 50% input.png output.webp
```

### Batch Conversion

Convert all PNG files to WebP:

```bash
scripts/convert.sh -f webp *.png
```

Output files will have the same basename with the new extension.

## Command Options

| Option          | Description                 | Example                |
| --------------- | --------------------------- | ---------------------- |
| `-f, --format`  | Output format               | `-f webp`              |
| `-q, --quality` | Quality 1-100 (default: 85) | `-q 90`                |
| `-r, --resize`  | Resize geometry             | `-r 800x600`, `-r 50%` |
| `-v, --verbose` | Show conversion commands    | `-v`                   |
| `-h, --help`    | Show help                   | `-h`                   |

## Resize Geometry

| Syntax | Description                     | Example   |
| ------ | ------------------------------- | --------- |
| `WxH`  | Exact dimensions (may crop)     | `800x600` |
| `Wx`   | Width only, height proportional | `1920x`   |
| `xH`   | Height only, width proportional | `x1080`   |
| `W%`   | Percentage of original          | `50%`     |

## Tool Dependencies

The script uses available tools in this priority:

1. **ImageMagick** (`magick` or `convert`): Primary tool for all formats
   - Prefers `magick` (ImageMagick 7+)
   - Falls back to `convert` (legacy ImageMagick 6) if needed
2. **cwebp/dwebp**: Optimized WebP conversion when available
3. **sips** (macOS): Can be used as fallback on macOS

Install ImageMagick for full functionality:

```bash
# macOS
brew install imagemagick webp

# Ubuntu/Debian
sudo apt-get install imagemagick webp

# Arch
sudo pacman -S imagemagick libwebp
```

## Common Workflows

### Optimize Images for Web

Convert to WebP with good quality and reasonable size:

```bash
scripts/convert.sh -f webp -q 80 -r 1200x *.png
```

### Create Thumbnails

Generate 300px wide thumbnails:

```bash
for img in *.jpg; do
    scripts/convert.sh -f jpg -q 75 -r 300x "$img" "thumbs/$img"
done
```

### Convert HEIC to JPEG

```bash
scripts/convert.sh -f jpg -q 95 IMG_1234.HEIC output.jpg
```

### PNG to Lossy for Smaller Size

```bash
scripts/convert.sh -f webp -q 85 large.png small.webp
```

## Tips

- **WebP**: Best for web - 25-35% smaller than JPEG with same quality
- **AVIF**: Best compression but slower encoding, limited support
- **PNG**: Use for graphics with transparency or when lossless is required
- **JPEG**: Good for photos when WebP/AVIF aren't options
- Quality 80-85: Good balance of size and quality
- Quality 90-95: High quality for important images
- Quality 60-75: Acceptable for thumbnails/previews
