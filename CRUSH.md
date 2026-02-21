# CRUSH Configuration for Christian's Dotfiles

## Overview

This is a hybrid dotfiles repository that uses direct symlinks for
configuration management rather than Nix/Home Manager. The repository supports
multiple platforms and uses different package managers depending on the OS.

## Platform Support

- **macOS** (aarch64 and x86_64): Homebrew for packages, direct symlinks for configs
- **NixOS** (x86_64): Nix for system packages, direct symlinks for user configs
- **Fedora**: dnf for packages, direct symlinks for configs
- **Other Linux**: System package manager (e.g., apt, pacman) + direct symlinks
- **WSL**: Treats as Linux

## Setup & Management

### Initial Setup

```bash
# Clone the repo
git clone <repo-url> ~/.dotfiles
cd ~/.dotfiles

# Install and link everything
./setup link

# Update all dependencies (installs tools via Homebrew, etc.)
./setup update
```

### Symlink Management

The `setup` script manages configuration files via symlinks to this repository:

```bash
# Link all configs (backs up existing files)
./setup link

# Remove all symlinks
./setup remove

# Update dependencies
./setup update
```

### Nix (for existing Nix systems only)

The flake exists primarily to remove Nix/Home Manager packages from existing systems:

```bash
# Enable flakes on non-NixOS systems
./enable-flakes

# Remove home-manager packages and clean up
nix run

# For NixOS systems (system configuration)
nixos-rebuild switch --flake .

# Deploy from remote
nixos-rebuild switch --flake github:meowgorithm/dotfiles/master
```

### Skills

Skills go in the ./crush/skills directory, which is symlinked to
~/.config/crush/skills. When creating or managing skills, do not try and put
them anywhere else.

**Note:** Do not create `.skill` packaged files in this repository. The
`package_skill.py` script creates these for distribution, but for personal
use the unpacked skill directories are sufficient. Keep only the source
files (SKILL.md, scripts/, etc.) in the repo.

## Architecture

### Configuration Structure

All user configurations are managed as direct symlinks from the repository:

- **Shell**: `bash/` → `~/.bashrc`, `~/.bash_profile`, `~/.profile`
- **Editors**: `nvim/`, `vim/`, `helix/` → `~/.config/`
- **Terminal**: `kitty/`, `ghostty/` → `~/.config/`
- **Window Manager (NixOS)**: `hypr/`, `waybar/` → `~/.config/`
- **Tools**: `tmux/`, `git/`, `ssh/`, `prettier/`, `fourmolu/` → various locations
- **Scripts**: `scripts/` → `~/.bin/`

### Package Management

**macOS**:

- Uses `Brewfile` for all package management
- Includes Homebrew formulas and casks
- Font management via copying

**NixOS**:

- System packages defined in `modules/nixos/default.nix`
- Only for system-level tools and services
- User packages are NOT managed via Nix

**Fedora**:

- Uses dnf for system package management
- `./setup update` installs tools via dnf
- `scripts/dnf-bundle` manages packages declaratively (like Homebrew's Brewfile)
- User configs managed via direct symlinks

**Other Linux**:

- Uses system package manager (apt, pacman, etc.)
- Manual installation of tools via `./setup update`

### Functions & Utilities

Common bash functions available in `bash/bash_funcs`:

- `num_cores()`: Returns number of CPU cores
- `which_os()`: Detects OS (darwin, nixos, fedora, void, arch, debian, linux)
- `command_exists()`: Checks if a command exists
- `getCharmRepos()`: Lists all Charm repos
- `pickCharmRepo()`: Interactive repo selection with gum

## Supported Systems

### Configured Hostnames

From `flake.nix`:

- **artemis** - NixOS desktop (x86_64-linux)
- **whitenoise** - NixOS desktop (x86_64-linux)
- **pantera** - Mac Studio (aarch64-darwin)
- **meowmachine** - MacBook Pro (aarch64-darwin)
- **la-tigra** - MacBook Air (aarch64-darwin)
- **wsl** - WSL (x86_64-linux, user: chris)

### Desktop Environments

- **NixOS**: Hyprland + Waybar
- **macOS**: Native desktop with Kitty/Ghostty terminals

## Important Notes

### Nix is Being Phased Out

Nix and Home Manager are deprecated for user configuration. The flake only exists to:

- Remove existing Nix/Home Manager packages from systems
- Provide NixOS system configuration (Hyprland, system packages, services)
- Eventually be removed entirely

### Workflow

1. **Add new config**: Add file to repository, run `./setup link`
2. **Remove config**: Delete symlink, run `./setup remove` if needed
3. **Update tools**: Run `./setup update` to install/update dependencies

### Migration Path

For systems still using Nix/Home Manager:

1. Run `nix run` to remove home-manager packages
2. Run `./setup link` to establish symlink-based configs
3. Eventually remove Nix entirely (except for NixOS systems)

## Editor Configuration

### Neovim

- Located in `nvim/init.lua`
- LSP setup, colorschemes (charmtone, pantera-negra)
- Snippets in `nvim/vsnip/`

### Vim

- Located in `vim/vimrc`
- Traditional vim configuration
- Plugins managed via `vim/plugged/`

### Helix

- Located in `helix/config.toml`
- Language configuration in `helix/languages.toml`
- Themes: maas, charm

## Key Tools & Scripts

### Scripts in `~/.bin/`

- `tm`, `tmls`: tmux utilities
- `sessions`: Session management
- `install-stuff`: Install additional tools
- `fix-docker-desktop`: Fix Docker Desktop issues
- `setup-mouse`, `setup-wacom`: Peripheral setup

### GPG

- `gpg-send`, `gpg-recv`: Key sharing utilities
- `gpg-edit-key`: Edit GPG keys
- Encrypted environment files in `pantera/`

### Development Tools

- Format: Fourmolu (Haskell), Prettier, shfmt, stylua, gofumpt
- LSP: Language servers for Go, Lua, Python, etc.
- Git: Configured with useful aliases and hooks

## Fonts

Custom fonts managed via `fonts/` directory:

- Copied to `~/Library/Fonts/Meowgorithm` on macOS via `./setup`
- Includes: IBM Plex, Inter, JetBrains Mono, and custom fonts

## Commit Style

All commit messages should:

- Be one line
- Be under 78 columns
- Include Crush attribution when appropriate
