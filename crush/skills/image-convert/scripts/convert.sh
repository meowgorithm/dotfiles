#!/usr/bin/env bash

# Image conversion utility supporting multiple formats
# Usage: convert.sh [options] <input> [output]

set -e

# Default values
QUALITY=85
RESIZE=""
FORMAT=""
VERBOSE=false

# Show usage
usage() {
    cat << 'EOF'
Usage: convert.sh [options] <input> [output]

Options:
    -f, --format <format>    Output format (png, jpg, webp, gif, bmp, tiff, avif, heic)
    -q, --quality <1-100>    Quality for lossy formats (default: 85)
    -r, --resize <geometry>  Resize (e.g., 800x600, 50%, 1920x)
    -v, --verbose            Verbose output
    -h, --help               Show this help

Examples:
    convert.sh input.png output.jpg
    convert.sh -f webp -q 90 input.png output.webp
    convert.sh -f jpg -r 1920x input.png output.jpg
    convert.sh -f webp *.png                    # Batch convert
EOF
    exit 0
}

# Check dependencies
check_deps() {
    local has_im=false
    local has_cwebp=false
    local has_dwebp=false
    
    if command -v convert &>/dev/null || command -v magick &>/dev/null; then
        has_im=true
    fi
    
    if command -v cwebp &>/dev/null; then
        has_cwebp=true
    fi
    
    if command -v dwebp &>/dev/null; then
        has_dwebp=true
    fi
    
    echo "$has_im|$has_cwebp|$has_dwebp"
}

# Get ImageMagick command
# Prefers 'magick' (ImageMagick 7+) over deprecated 'convert'
get_im_cmd() {
    if command -v magick &>/dev/null; then
        echo "magick"
    else
        echo "convert"
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

# Convert using ImageMagick
convert_with_im() {
    local input="$1"
    local output="$2"
    local format="$3"
    local quality="$4"
    local resize="$5"
    
    local cmd=($(get_im_cmd))
    cmd+=("$input")
    
    if [[ -n "$resize" ]]; then
        cmd+=("-resize" "$resize")
    fi
    
    if [[ "$format" == "jpg" || "$format" == "jpeg" ]]; then
        cmd+=("-quality" "$quality")
    elif [[ "$format" == "webp" ]]; then
        cmd+=("-quality" "$quality" "-define" "webp:target-quality=$quality")
    fi
    
    cmd+=("$output")
    
    if [[ "$VERBOSE" == true ]]; then
        echo "Running: ${cmd[*]}"
    fi
    
    "${cmd[@]}"
}

# Convert WebP with cwebp (better quality/size ratio)
convert_to_webp() {
    local input="$1"
    local output="$2"
    local quality="$3"
    local resize="$4"
    
    local opts=()
    
    if [[ -n "$resize" ]]; then
        # Parse resize geometry (e.g., "800x600" or "50%")
        if [[ "$resize" == *%* ]]; then
            local percent="${resize%%%}"
            opts+=("-size" "${percent}%")
        else
            opts+=("-resize" "$resize")
        fi
    fi
    
    opts+=("-q" "$quality" "$input" "-o" "$output")
    
    if [[ "$VERBOSE" == true ]]; then
        echo "Running: cwebp ${opts[*]}"
    fi
    
    cwebp "${opts[@]}"
}

# Convert from WebP with dwebp
convert_from_webp() {
    local input="$1"
    local output="$2"
    local resize="$3"
    
    local opts=()
    
    if [[ -n "$resize" ]]; then
        # dwebp doesn't support resize directly, use ImageMagick
        convert_with_im "$input" "$output" "" "" "$resize"
        return
    fi
    
    opts+=("$input" "-o" "$output")
    
    if [[ "$VERBOSE" == true ]]; then
        echo "Running: dwebp ${opts[*]}"
    fi
    
    dwebp "${opts[@]}"
}

# Detect output format from filename
detect_format() {
    local output="$1"
    local ext
    ext=$(get_extension "$output")
    
    case "$ext" in
        png) echo "png" ;;
        jpg|jpeg) echo "jpg" ;;
        webp) echo "webp" ;;
        gif) echo "gif" ;;
        bmp) echo "bmp" ;;
        tiff|tif) echo "tiff" ;;
        avif) echo "avif" ;;
        heic|heif) echo "heic" ;;
        *) echo "" ;;
    esac
}

# Convert a single file
convert_file() {
    local input="$1"
    local output="$2"
    
    if [[ ! -f "$input" ]]; then
        echo "Error: Input file not found: $input" >&2
        return 1
    fi
    
    local input_ext
    input_ext=$(get_extension "$input")
    
    # Auto-detect output format if not specified
    if [[ -z "$FORMAT" && -n "$output" ]]; then
        FORMAT=$(detect_format "$output")
    fi
    
    if [[ -z "$FORMAT" ]]; then
        echo "Error: Could not detect output format from: $output" >&2
        return 1
    fi
    
    # Determine output filename
    if [[ -z "$output" ]]; then
        output="$(get_basename "$input").$FORMAT"
    fi
    
    # Create output directory if needed
    local out_dir
    out_dir=$(dirname "$output")
    if [[ "$out_dir" != "." && ! -d "$out_dir" ]]; then
        mkdir -p "$out_dir"
    fi
    
    # Use specialized tools when available
    if [[ "$input_ext" == "webp" && "$FORMAT" != "webp" ]]; then
        local deps
        deps=$(check_deps)
        if [[ "$deps" == *"|true|true"* ]] && [[ "$FORMAT" =~ ^(png|ppm|pam|pgm)$ ]]; then
            convert_from_webp "$input" "$output" "$RESIZE"
            return
        fi
    fi
    
    if [[ "$FORMAT" == "webp" && "$input_ext" != "webp" ]]; then
        local deps
        deps=$(check_deps)
        if [[ "$deps" == *"|true|"* ]]; then
            convert_to_webp "$input" "$output" "$QUALITY" "$RESIZE"
            return
        fi
    fi
    
    # Fallback to ImageMagick
    local deps
    deps=$(check_deps)
    if [[ "$deps" == *"true|"* ]]; then
        convert_with_im "$input" "$output" "$FORMAT" "$QUALITY" "$RESIZE"
    else
        echo "Error: ImageMagick not found. Please install ImageMagick." >&2
        return 1
    fi
}

# Main
main() {
    local inputs=()
    local output=""
    
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -f|--format)
                FORMAT="$2"
                shift 2
                ;;
            -q|--quality)
                QUALITY="$2"
                shift 2
                ;;
            -r|--resize)
                RESIZE="$2"
                shift 2
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -h|--help)
                usage
                ;;
            -*)
                echo "Unknown option: $1" >&2
                exit 1
                ;;
            *)
                if [[ ${#inputs[@]} -eq 0 && -z "$output" ]]; then
                    inputs+=("$1")
                elif [[ -z "$output" && "$1" != -* ]]; then
                    # Check if this looks like an output file (has extension different from input)
                    local in_ext out_ext
                    in_ext=$(get_extension "${inputs[0]}")
                    out_ext=$(get_extension "$1")
                    if [[ "$in_ext" != "$out_ext" ]] || [[ -n "$FORMAT" ]]; then
                        output="$1"
                    else
                        inputs+=("$1")
                    fi
                else
                    inputs+=("$1")
                fi
                shift
                ;;
        esac
    done
    
    if [[ ${#inputs[@]} -eq 0 ]]; then
        echo "Error: No input files specified" >&2
        usage
    fi
    
    # Batch conversion mode
    if [[ ${#inputs[@]} -gt 1 && -z "$output" ]]; then
        if [[ -z "$FORMAT" ]]; then
            echo "Error: Format required for batch conversion. Use -f <format>" >&2
            exit 1
        fi
        
        for input in "${inputs[@]}"; do
            output="$(get_basename "$input").$FORMAT"
            echo "Converting: $input -> $output"
            convert_file "$input" "$output"
        done
        return
    fi
    
    # Single file conversion
    if [[ ${#inputs[@]} -eq 1 ]]; then
        convert_file "${inputs[0]}" "$output"
    fi
}

main "$@"
