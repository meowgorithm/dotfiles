#!/usr/bin/env bash
#
# Convert video files to animated WebP.
#
# Primary path: ffmpeg decodes frames to a temp dir, then img2webp
#   (from libwebp) assembles them. This works even with ffmpeg builds
#   that lack the libwebp encoder (e.g. recent Homebrew ffmpeg).
# Fallback: ffmpeg's libwebp encoder, if available.
#
# Usage: to-webp.sh [options] <input> [output]

set -e

# Defaults
FPS=15
WIDTH=""
QUALITY=75
LOOP=0
COMPRESSION=6
START=""
DURATION=""
OVERWRITE=false
LOSSLESS=false

usage() {
    cat <<'EOF'
Usage: to-webp.sh [options] <input> [output]

Options:
    -f, --fps N          Frames per second (default: 15)
    -w, --width N        Output width in px (height auto, keeps aspect)
    -q, --quality N      Quality 0-100 (default: 75)
    -l, --loop N         Loop count, 0=infinite (default: 0)
    -c, --compression N  Compression effort 0-6 (default: 6; higher=smaller)
    -s, --start TIME     Start time (seconds or HH:MM:SS)
    -d, --duration TIME  Max duration (seconds or HH:MM:SS)
    --lossless           Use lossless encoding (larger files)
    -y, --yes            Overwrite output without asking
    -h, --help           Show this help

Examples:
    to-webp.sh demo.mp4                          # -> demo.webp (15fps, q75)
    to-webp.sh -f 24 -w 800 demo.mp4             # 24fps, 800px wide
    to-webp.sh -q 90 -w 480 clip.mov out.webp    # high quality thumbnail
    to-webp.sh -s 3 -d 5 movie.mp4 clip.webp     # 5s clip starting at 3s
    to-webp.sh -y *.mp4                          # batch convert, overwrite
EOF
    exit 0
}

have() { command -v "$1" &>/dev/null; }

check_deps() {
    if ! have ffmpeg; then
        echo "Error: ffmpeg not found. Install ffmpeg." >&2
        echo "  macOS: brew install ffmpeg" >&2
        echo "  Ubuntu/Debian: sudo apt-get install ffmpeg" >&2
        echo "  Fedora: sudo dnf install ffmpeg" >&2
        echo "  Arch: sudo pacman -S ffmpeg" >&2
        return 1
    fi

    if have img2webp; then
        return 0
    fi

    # Fall back to ffmpeg's libwebp encoder if img2webp isn't installed.
    if ffmpeg -hide_banner -h encoder=libwebp >/dev/null 2>&1; then
        return 0
    fi

    echo "Error: need img2webp (from libwebp) or an ffmpeg with libwebp." >&2
    echo "  macOS: brew install webp" >&2
    echo "  Ubuntu/Debian: sudo apt-get install webp" >&2
    echo "  Fedora: sudo dnf install libwebp-tools" >&2
    echo "  Arch: sudo pacman -S libwebp" >&2
    return 1
}

get_basename() { echo "${1%.*}"; }

# Convert via img2webp: ffmpeg -> PNG frames -> img2webp -> animated webp.
convert_via_img2webp() {
    local input="$1" output="$2"

    local tmpdir
    tmpdir=$(mktemp -d -t to-webp.XXXXXX)
    # shellcheck disable=SC2064
    trap "rm -rf '$tmpdir'" RETURN

    # Build video filter chain
    local vf="fps=${FPS}"
    if [[ -n "$WIDTH" ]]; then
        vf+=",scale=${WIDTH}:-2:flags=lanczos"
    fi

    # Extract frames as PNGs
    local ff=(ffmpeg -hide_banner -loglevel error -y)
    if [[ -n "$START" ]]; then ff+=(-ss "$START"); fi
    ff+=(-i "$input")
    if [[ -n "$DURATION" ]]; then ff+=(-t "$DURATION"); fi
    ff+=(-vf "$vf" -an "$tmpdir/%06d.png")

    "${ff[@]}"

    # Collect frames (globbing avoided in case shopt isn't set)
    local frames=()
    while IFS= read -r -d '' f; do
        frames+=("$f")
    done < <(find "$tmpdir" -name '*.png' -print0 | sort -z)

    if [[ ${#frames[@]} -eq 0 ]]; then
        echo "Error: ffmpeg produced no frames." >&2
        return 1
    fi

    # Frame duration in ms (integer); img2webp requires >= 1ms.
    local frame_ms=$((1000 / FPS))
    [[ "$frame_ms" -lt 1 ]] && frame_ms=1

    # File-level options first, then per-frame options, then frames.
    # img2webp defaults to lossless, so -lossy must be set explicitly.
    local iw=(img2webp -loop "$LOOP")
    iw+=(-d "$frame_ms" -m "$COMPRESSION")
    if [[ "$LOSSLESS" == true ]]; then
        iw+=(-lossless)
    else
        iw+=(-lossy -q "$QUALITY")
    fi
    iw+=("${frames[@]}" -o "$output")

    "${iw[@]}" >/dev/null
}

# Convert via ffmpeg's native libwebp encoder (fallback).
convert_via_ffmpeg() {
    local input="$1" output="$2"

    local vf="fps=${FPS}"
    if [[ -n "$WIDTH" ]]; then
        vf+=",scale=${WIDTH}:-2:flags=lanczos"
    fi

    local cmd=(ffmpeg -hide_banner -loglevel error -y)
    if [[ -n "$START" ]]; then cmd+=(-ss "$START"); fi
    cmd+=(-i "$input")
    if [[ -n "$DURATION" ]]; then cmd+=(-t "$DURATION"); fi

    cmd+=(
        -vf "$vf"
        -an
        -c:v libwebp
        -loop "$LOOP"
        -compression_level "$COMPRESSION"
        -preset picture
    )

    if [[ "$LOSSLESS" == true ]]; then
        cmd+=(-lossless 1)
    else
        cmd+=(-lossless 0 -q:v "$QUALITY")
    fi

    cmd+=(-fps_mode vfr "$output")

    "${cmd[@]}"
}

convert_file() {
    local input="$1" output="$2"

    if [[ ! -f "$input" ]]; then
        echo "Error: Input file not found: $input" >&2
        return 1
    fi

    if [[ -z "$output" ]]; then
        output="$(get_basename "$input").webp"
    fi

    if [[ -f "$output" && "$OVERWRITE" == false ]]; then
        read -r -p "Output exists: $output. Overwrite? [y/N] " response
        if [[ ! "$response" =~ ^[Yy]$ ]]; then
            echo "Skipping: $input"
            return 0
        fi
    fi

    local out_dir
    out_dir=$(dirname "$output")
    if [[ "$out_dir" != "." && ! -d "$out_dir" ]]; then
        mkdir -p "$out_dir"
    fi

    echo "Converting: $input -> $output"

    if have img2webp; then
        convert_via_img2webp "$input" "$output"
    else
        convert_via_ffmpeg "$input" "$output"
    fi

    echo "Created: $output ($(du -h "$output" | cut -f1))"
}

main() {
    local inputs=() output=""

    while [[ $# -gt 0 ]]; do
        case "$1" in
        -f | --fps) FPS="$2"; shift 2 ;;
        -w | --width) WIDTH="$2"; shift 2 ;;
        -q | --quality) QUALITY="$2"; shift 2 ;;
        -l | --loop) LOOP="$2"; shift 2 ;;
        -c | --compression) COMPRESSION="$2"; shift 2 ;;
        -s | --start) START="$2"; shift 2 ;;
        -d | --duration) DURATION="$2"; shift 2 ;;
        --lossless) LOSSLESS=true; shift ;;
        -y | --yes) OVERWRITE=true; shift ;;
        -h | --help) usage ;;
        -*)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
        *)
            inputs+=("$1")
            shift
            ;;
        esac
    done

    if [[ ${#inputs[@]} -eq 0 ]]; then
        echo "Error: No input files specified" >&2
        usage
    fi

    check_deps

    # If 2 args and second ends in .webp, treat as explicit output.
    if [[ ${#inputs[@]} -eq 2 ]]; then
        local last_lc
        last_lc=$(echo "${inputs[1]}" | tr '[:upper:]' '[:lower:]')
        if [[ "$last_lc" == *.webp ]]; then
            output="${inputs[1]}"
            inputs=("${inputs[0]}")
        fi
    fi

    if [[ ${#inputs[@]} -gt 1 ]]; then
        for input in "${inputs[@]}"; do
            convert_file "$input" ""
        done
        return 0
    fi

    convert_file "${inputs[0]}" "$output"
}

main "$@"
