---
name: video-mute
description: Remove audio tracks from video files. Use when the user needs to strip audio from videos, create silent versions, or remove unwanted soundtracks from MP4, MOV, AVI, MKV, WebM, and other video formats.
---

# Video Mute

Remove audio tracks from video files while preserving video quality.

## Supported Formats

| Format | Extension | Notes                            |
| ------ | --------- | -------------------------------- |
| MP4    | .mp4      | Most common, widely compatible   |
| MOV    | .mov      | Apple QuickTime format           |
| AVI    | .avi      | Windows format, larger files     |
| MKV    | .mkv      | Matroska, supports many codecs   |
| WebM   | .webm     | Web-optimized format             |
| MPEG   | .mpg, .mpeg | Older format                   |
| FLV    | .flv      | Flash video format               |
| WMV    | .wmv      | Windows Media Video              |

## Quick Start

### Remove Audio from Single File

```bash
scripts/mute.sh input.mp4 output.mp4
```

Or use the default naming (appends `-muted`):

```bash
scripts/mute.sh input.mp4
# Creates: input-muted.mp4
```

### Batch Process Multiple Files

Remove audio from all MP4 files in current directory:

```bash
scripts/mute.sh *.mp4
```

Output files will be named with `-muted` suffix.

### Copy Instead of Re-encode (Fastest)

Use copy mode to strip audio without re-encoding video (much faster):

```bash
scripts/mute.sh -c input.mp4 output.mp4
```

### Specify Video Codec

```bash
# Re-encode with H.264 (better compatibility)
scripts/mute.sh -v libx264 input.mp4 output.mp4

# Re-encode with H.265/HEVC (smaller file size)
scripts/mute.sh -v libx265 input.mp4 output.mp4
```

## Command Options

| Option         | Description                              | Example             |
| -------------- | ---------------------------------------- | ------------------- |
| `-c, --copy`   | Copy video stream (no re-encode, fast)   | `-c`                |
| `-v, --vcodec` | Video codec (default: copy)              | `-v libx264`        |
| `-y, --yes`    | Overwrite output without asking          | `-y`                |
| `-h, --help`   | Show help                                | `-h`                |

## Tool Dependencies

Requires **ffmpeg** for video processing.

Install ffmpeg:

```bash
# macOS
brew install ffmpeg

# Ubuntu/Debian
sudo apt-get install ffmpeg

# Arch
sudo pacman -S ffmpeg

# Fedora
dnf install ffmpeg
```

## Common Workflows

### Prepare Videos for Social Media

Remove original audio before adding new soundtrack:

```bash
scripts/mute.sh -c vacation.mp4
# Then add new audio with ffmpeg or video editor
```

### Batch Process Folder

Remove audio from all videos in a folder:

```bash
for video in ~/Videos/*.mp4; do
    scripts/mute.sh -c "$video"
done
```

### Create Preview Clips

Make short silent preview versions:

```bash
# First trim to 10 seconds, then mute
ffmpeg -i long-video.mp4 -t 10 -c copy -an preview.mp4
```

### Strip Audio for Privacy

Remove sensitive audio before sharing:

```bash
scripts/mute.sh -c -y meeting-recording.mp4
```

## Performance Tips

- **Copy mode (`-c`)**: Fastest, no quality loss, same file size
- **Re-encoding**: Slower, may reduce quality, can reduce file size
- Use copy mode when you just need to remove audio without other changes
- Re-encode only if you need to change video codec or compress further

## Troubleshooting

### "Unknown encoder" error

Some codecs may not be available. Check available codecs:

```bash
ffmpeg -encoders | grep -i h264
```

### Output file is larger than input

Use copy mode (`-c`) instead of re-encoding.

### Quality looks worse

Copy mode preserves original quality. If re-encoding, try a higher bitrate:

```bash
ffmpeg -i input.mp4 -an -vcodec libx264 -crf 18 output.mp4
```
