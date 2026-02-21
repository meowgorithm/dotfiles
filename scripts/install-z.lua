#!/usr/bin/env bash
set -euo pipefail

# Install z.lua from source.

# Abort on macOS (use Homebrew instead)
if [[ "$(uname -s)" == "Darwin" ]]; then
    echo "On macOS, install z.lua via Homebrew: brew install z.lua"
    exit 1
fi

installDir="$HOME/.local/share/z.lua"
mkdir -p "$installDir"

# Download z.lua
curl -fsSL https://raw.githubusercontent.com/skywind3000/z.lua/master/z.lua -o "$installDir/z.lua"
chmod +x "$installDir/z.lua"

echo "z.lua installed to $installDir/z.lua"
echo "Reload your shell or run: source ~/.bashrc"
