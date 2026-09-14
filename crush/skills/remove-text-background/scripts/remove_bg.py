#!/usr/bin/env python3
"""Remove a solid background color from an image (e.g. terminal/markdown screenshots).

Uses exact alpha uncompositing (GIMP-style "Color to Alpha"): for each pixel it
inverts C = a*F + (1-a)*B, so anti-aliased text edges keep their true color and
get a fractional alpha instead of a dark halo.

Usage:
    remove_bg.py INPUT OUTPUT [--bg R,G,B | --bg "#2d2c37"] [--corner x,y]

Background color defaults to the most common color among the four corners.
Output should be a format with alpha (PNG recommended).
"""
import argparse
import sys

try:
    import numpy as np
    from PIL import Image
except ImportError:
    sys.exit(
        "Missing dependencies. Install with: python3 -m pip install --user pillow numpy"
    )


def parse_color(s):
    s = s.strip()
    if s.startswith("#"):
        h = s.lstrip("#")
        if len(h) == 3:
            h = "".join(ch * 2 for ch in h)
        if len(h) != 6:
            raise argparse.ArgumentTypeError(f"Bad hex color: {s}")
        return tuple(int(h[i : i + 2], 16) for i in (0, 2, 4))
    parts = tuple(int(p) for p in s.split(","))
    if len(parts) != 3:
        raise argparse.ArgumentTypeError(f"Bad color: {s} (want R,G,B)")
    return parts


def corner_color(img, x=None, y=None):
    w, h = img.size
    pts = [(x, y)] if x is not None and y is not None else [
        (0, 0), (w - 1, 0), (0, h - 1), (w - 1, h - 1)
    ]
    colors = [img.getpixel(p) for p in pts]
    return max(set(colors), key=colors.count)


def main():
    ap = argparse.ArgumentParser(description="Remove solid background color to transparency.")
    ap.add_argument("input")
    ap.add_argument("output")
    ap.add_argument("--bg", type=parse_color, help="Background color as R,G,B or #hex")
    ap.add_argument("--corner", type=lambda s: tuple(int(v) for v in s.split(",")),
                    help="Pixel x,y to sample the background color from")
    args = ap.parse_args()

    img = Image.open(args.input).convert("RGB")
    if args.bg:
        bg = np.array(args.bg, dtype=np.float64)
    else:
        cx, cy = args.corner if args.corner else (None, None)
        bg = np.array(corner_color(img, cx, cy), dtype=np.float64)
        print(f"background: rgb({int(bg[0])},{int(bg[1])},{int(bg[2])})")

    c = np.asarray(img).astype(np.float64)

    # Per-channel minimum alpha for which C = a*F + (1-a)*B has a valid F.
    hi = np.where(255.0 - bg > 0, (c - bg) / np.maximum(255.0 - bg, 1e-9), 0.0)
    lo = np.where(bg > 0, (bg - c) / np.maximum(bg, 1e-9), 0.0)
    a = np.maximum(np.where(c >= bg, hi, lo).max(axis=2), 0.0)
    a = np.clip(a, 0.0, 1.0)

    # Recover foreground color.
    safe_a = np.maximum(a, 1e-9)[..., None]
    f = np.clip((c - (1.0 - safe_a) * bg) / safe_a, 0, 255)
    f[a <= 0.0] = 0

    out = np.dstack([f, a * 255.0]).astype(np.uint8)
    Image.fromarray(out).save(args.output)
    print(f"saved: {args.output} {img.size}")


if __name__ == "__main__":
    main()
