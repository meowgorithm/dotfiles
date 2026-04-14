#!/bin/bash
# Trim image border and add back a border of the same color

set -e

if [ $# -lt 3 ]; then
    echo "Usage: $0 <input> <border-px> <output>"
    echo "Example: $0 image.png 75 image-bordered.png"
    exit 1
fi

input="$1"
border_px="$2"
output="$3"

# Get the border color from top-left corner
color=$(magick "$input"[1x1+0+0] -format "%[pixel:s]" info:)

# Trim the existing border and add new border
magick "$input" -trim +repage -bordercolor "$color" -border "$border_px" "$output"

echo "Created $output with ${border_px}px border of $color"
