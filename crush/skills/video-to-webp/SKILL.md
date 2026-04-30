---
name: video-to-webp
description: Convert video files (MP4, MOV, WebM, GIF, MKV, AVI, etc.) into animated WebP images with control over fps, size, quality, loop count, and trimming. Use when the user wants to turn a video or screen recording into an animated WebP, create a lightweight demo loop, replace an animated GIF with a smaller WebP, or produce a short clip as a .webp.
---

# Video to Animated WebP

Convert videos into animated WebP. Under the hood the script uses
ffmpeg to decode and resample frames, then `img2webp` (from libwebp)
to assemble the animation. This works even when Homebrew's ffmpeg is
built without the `libwebp` encoder. If `img2webp` is missing, the
script falls back to ffmpeg's `libwebp` encoder when available.

Animated WebP is typically 30-80% smaller than an equivalent GIF and
supports full 24-bit color and alpha.

## Supported Inputs

Anything ffmpeg can decode: MP4, MOV, WebM, MKV, AVI, GIF, FLV, WMV,
MPEG, M4V, etc.

## Quick Start

Auto-named output (`input.mp4` -> `input.webp`):

```bash
scripts/to-webp.sh input.mp4
```

Explicit output file:

```bash
scripts/to-webp.sh input.mp4 output.webp
```

Higher fps and constrained width:

```bash
scripts/to-webp.sh -f 24 -w 800 demo.mov demo.webp
```

Trim to a 5-second clip starting at 00:00:03:

```bash
scripts/to-webp.sh -s 00:00:03 -d 5 movie.mp4 clip.webp
```

Batch convert everything in a directory:

```bash
scripts/to-webp.sh -y *.mp4
```

## Command Options

| Option               | Description                                  | Default |
| -------------------- | -------------------------------------------- | ------- |
| `-f, --fps N`        | Frames per second                            | `15`    |
| `-w, --width N`      | Output width in px (height scales, auto even)| source  |
| `-q, --quality N`    | Quality 0-100 (ignored with `--lossless`)    | `75`    |
| `-l, --loop N`       | Loop count, `0` = infinite                   | `0`     |
| `-c, --compression N`| Compression effort 0-6 (higher = smaller)    | `6`     |
| `-s, --start TIME`   | Start offset (seconds or `HH:MM:SS`)         | —       |
| `-d, --duration T`   | Max duration (seconds or `HH:MM:SS`)         | —       |
| `--lossless`         | Use lossless encoding (much larger)          | off     |
| `-y, --yes`          | Overwrite output without prompting           | off     |
| `-h, --help`         | Show help                                    |         |

## Tuning Guide

Pick fps and width based on intent:

| Use case               | fps   | width  | quality | Notes                        |
| ---------------------- | ----- | ------ | ------- | ---------------------------- |
| README demo / TTY cast | 10-15 | 800    | 70-80   | Smooth enough, small file    |
| UI interaction clip    | 24-30 | 960    | 75-85   | Keeps motion fluid           |
| Social / thumbnail     | 15-20 | 480    | 70      | Autoplays quickly            |
| GIF replacement        | match | match  | 75      | Smaller than source GIF      |
| Archival / no loss     | 24+   | source | —       | Use `--lossless`             |

General rules:

- Cutting fps is the single biggest size win; drop to 10-15 fps unless
  the motion demands more.
- Width is the second lever; halving width roughly quarters bytes.
- Quality 60-75 is usually indistinguishable at small sizes.
- `-compression_level 6` is slow to encode but produces the smallest
  file; lower it only if encoding speed matters.

## Tool Dependencies

Requires **ffmpeg** (any recent build) and **libwebp** tools
(`img2webp`, `webpmux`).

```bash
# macOS
brew install ffmpeg webp

# Ubuntu/Debian
sudo apt-get install ffmpeg webp

# Fedora
sudo dnf install ffmpeg libwebp-tools

# Arch
sudo pacman -S ffmpeg libwebp
```

Verify:

```bash
command -v ffmpeg img2webp webpmux
```

## Common Workflows

### Convert a screen recording for a README

```bash
scripts/to-webp.sh -f 15 -w 800 -q 75 recording.mov demo.webp
```

### Replace an existing animated GIF

```bash
scripts/to-webp.sh big.gif big.webp
```

### Extract a short loop from a longer video

```bash
scripts/to-webp.sh -s 00:01:10 -d 4 -f 20 -w 640 talk.mp4 loop.webp
```

### Lossless capture (debugging, pixel-perfect)

```bash
scripts/to-webp.sh --lossless -f 30 -w 1280 capture.mp4 capture.webp
```

### Batch convert a folder of clips

```bash
for v in ~/Videos/clips/*.mp4; do
    scripts/to-webp.sh -y -f 15 -w 720 "$v"
done
```

## Troubleshooting

### "need img2webp (from libwebp) or an ffmpeg with libwebp"

Install the libwebp tools (`brew install webp`, `apt-get install
webp`, `dnf install libwebp-tools`, or `pacman -S libwebp`). This is
the preferred path because recent Homebrew ffmpeg builds no longer
ship the `libwebp` encoder.

### Output file is huge

- Lower `--fps` (e.g. 10-15).
- Constrain `--width` (e.g. 640 or 480).
- Lower `--quality` (e.g. 60).
- Make sure `--lossless` is not set.
- Trim with `--start` / `--duration` — animated WebP scales linearly
  with frame count.

### Playback is jerky

Raise `--fps` (24-30) or avoid downscaling too aggressively. Some
viewers also render animated WebP poorly; test in a browser.

### Colors look washed out

Lossy WebP uses YUV420 by default. Use `--lossless` if exact colors
matter (e.g. UI screenshots with fine gradients).

### Want to convert animated WebP back to video

```bash
ffmpeg -i input.webp output.mp4
```
