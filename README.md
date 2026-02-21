# The Meowgorithm Dotfiles, Collectors' Edition

This repo contains configuration files managed for:

- macOS (aarch64 and x86_64)
- Linux (Fedora, NixOS, and others, including WSL)

## Quick Start

```bash
# Clone the repo
git clone <repo-url> ~/.dotfiles
cd ~/.dotfiles

# Link all configs
./setup link

# Update dependencies (installs Nix, Homebrew packages, etc.)
./setup update
```

## Management

```bash
# Link all configs (backs up existing files)
./setup link

# Remove all symlinks
./setup remove

# Update dependencies
./setup update
```

## Package Management

- **macOS**: Homebrew via `Brewfile`
- **Fedora**: dnf via `DNFfile`
- **All platforms**: Nix profile via `nix-profile/flake.nix`

See `CRUSH.md` for more details.
