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

### Nix Profile

Package installation is managed via `nix-profile/flake.nix` using the modern `nix profile` command. This handles tools like `fourmolu`, `cabal-fmt`, and other packages previously installed via `cabal`.

The `setup update` command automatically:
1. Installs Nix if not present (via `scripts/install-nix`)
2. Upgrades all profile packages: `nix profile upgrade --all`
3. Or installs them initially: `nix profile install ./nix-profile`

```bash
# Manual nix-profile management
nix profile install ./nix-profile    # First-time install
nix profile upgrade --all            # Upgrade all packages
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
- Nix profile for Haskell tools (fourmolu, cabal-fmt, etc.)
- Font management via copying

**NixOS**:

- User packages are NOT managed via Nix (use nix-profile instead)
- `modules/nixos/` is being phased out

**Fedora**:

- Uses dnf for system package management
- `./setup update` installs tools via dnf
- Nix profile for Haskell tools (fourmolu, cabal-fmt, etc.)
- `scripts/dnf-bundle` manages packages declaratively (like Homebrew's Brewfile)
- User configs managed via direct symlinks

**Other Linux**:

- Uses system package manager (apt, pacman, etc.)
- Nix profile for Haskell tools (fourmolu, cabal-fmt, etc.)
- Manual installation of tools via `./setup update`

### Functions & Utilities

Common bash functions available in `bash/bash_funcs`:

- `num_cores()`: Returns number of CPU cores
- `which_os()`: Detects OS (darwin, nixos, fedora, void, arch, debian, linux)
- `command_exists()`: Checks if a command exists
- `getCharmRepos()`: Lists all Charm repos
- `pickCharmRepo()`: Interactive repo selection with gum

## Supported Systems

- **macOS** (aarch64 and x86_64): Homebrew for packages
- **Fedora**: dnf for packages
- **Other Linux**: System package manager + nix profile

### Desktop Environments

- **macOS**: Native desktop with Kitty/Ghostty terminals
- **Linux**: Hyprland + Waybar (config still present but NixOS module deprecated)

## Important Notes

### Workflow

1. **Add new config**: Add file to repository, run `./setup link`
2. **Remove config**: Delete symlink, run `./setup remove` if needed
3. **Update tools**: Run `./setup update` to install/update dependencies



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
- `install-nix`: Install the Nix package manager
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
