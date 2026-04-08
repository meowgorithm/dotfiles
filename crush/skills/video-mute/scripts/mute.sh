#!/usr/bin/env bash
#
# Remove audio from video files
# Usage: mute.sh [options] <input> [output]

set -e

# Default values
VCODEC="copy"
OVERWRITE=false

# Show usage
usage() {
    cat <<'EOF'
Usage: mute.sh [options] <input> [output]

Options:
    -c, --copy       Copy video stream without re-encoding (default, fastest)
    -v, --vcodec     Video codec for re-encoding (e.g., libx264, libx265)
    -y, --yes        Overwrite output without asking
    -h, --help       Show this help

Examples:
    mute.sh input.mp4 output.mp4         # Remove audio, copy video
    mute.sh input.mp4                    # Creates input-muted.mp4
    mute.sh -c input.mp4                 # Same as above (explicit copy)
    mute.sh -v libx264 input.mp4         # Re-encode with H.264
    mute.sh -y *.mp4                     # Batch mute, overwrite existing
EOF
    exit 0
}

# Check if ffmpeg is installed
check_deps() {
    if ! command -v ffmpeg &>/dev/null; then
        echo "Error: ffmpeg not found. Please install ffmpeg." >&2
        echo "  macOS: brew install ffmpeg" >&2
        echo "  Ubuntu/Debian: sudo apt-get install ffmpeg" >&2
        echo "  Arch: sudo pacman -S ffmpeg" >&2
        return 1
    fi
}

# Get file extension (lowercase)
get_extension() {
    local filename="$1"
    echo "${filename##*.}" | tr '[:upper:]' '[:lower:]'
}

# Get basename without extension
get_basename() {
    local filename="$1"
    echo "${filename%.*}"
}

# Remove audio from a single file
mute_file() {
    local input="$1"
    local output="$2"

    if [[ ! -f "$input" ]]; then
        echo "Error: Input file not found: $input" >&2
        return 1
    fi

    # Auto-generate output name if not provided
    if [[ -z "$output" ]]; then
        local ext
        ext=$(get_extension "$input")
        output="$(get_basename "$input")-muted.$ext"
    fi

    # Check if output exists
    if [[ -f "$output" && "$OVERWRITE" == false ]]; then
        read -r -p "Output file exists: $output. Overwrite? [y/N] " response
        if [[ ! "$response" =~ ^[Yy]$ ]]; then
            echo "Skipping: $input"
            return 0
        fi
    fi

    # Create output directory if needed
    local out_dir
    out_dir=$(dirname "$output")
    if [[ "$out_dir" != "." && ! -d "$out_dir" ]]; then
        mkdir -p "$out_dir"
    fi

    echo "Removing audio: $input -> $output"

    # Build ffmpeg command
    local cmd=(ffmpeg -i "$input" -an -vcodec "$VCODEC")

    if [[ "$OVERWRITE" == true ]]; then
        cmd+=(-y)
    fi

    # Add faststart for web optimization when using copy mode
    if [[ "$VCODEC" == "copy" ]]; then
        cmd+=(-movflags +faststart)
    fi

    cmd+=("$output")

    "${cmd[@]}"

    echo "Created: $output"
}

# Main
main() {
    local inputs=()
    local output=""

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
        -c | --copy)
            VCODEC="copy"
            shift
            ;;
        -v | --vcodec)
            VCODEC="$2"
            shift 2
            ;;
        -y | --yes)
            OVERWRITE=true
            shift
            ;;
        -h | --help)
            usage
            ;;
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

    # Check dependencies
    check_deps

    # Determine if last argument is an output file (single file mode)
    # Heuristic: single input with different extension = explicit output
    if [[ ${#inputs[@]} -eq 2 ]]; then
        local in_ext out_ext
        in_ext=$(get_extension "${inputs[0]}")
        out_ext=$(get_extension "${inputs[1]}")

        # If extensions differ, treat second as output
        if [[ "$in_ext" != "$out_ext" ]]; then
            output="${inputs[1]}"
            inputs=("${inputs[0]}")
        fi
    fi

    # Batch processing mode
    if [[ ${#inputs[@]} -gt 1 ]]; then
        for input in "${inputs[@]}"; do
            mute_file "$input" ""
        done
        return 0
    fi

    # Single file mode
    mute_file "${inputs[0]}" "$output"
}

main "$@"
