# CRUSH Configuration for Christian’s Dotfiles

## Build/Deploy Commands

```bash
# Enable flakes on non-NixOS systems
./enable-flakes

# Deploy home-manager configuration (default)
nix run

# Deploy for headless systems
nix run .#christian@headless

# Deploy NixOS system configuration
nixos-rebuild switch --flake .

# Deploy from remote
nixos-rebuild switch --flake github:meowgorithm/dotfiles/master

# Test configuration build without switching
nix build .#packages.x86_64-linux.default
```

Note: Nix commands you can, and are encouraged, to check the number of
system cores and use the `-j` flag to speed up builds, e.g. `nix build -j 32`.

## Architecture Notes

This repository is undergoing a transition from fully Nix-managed configurations 
to a hybrid approach where some tools are configured via direct symlinks rather 
than Home Manager modules. Recent commits have moved configurations for:

- Helix editor
- tmux
- Crush
- fourmolu
- Kitty terminal
- Git

Out of Home Manager modules. This simplifies the configuration structure and 
reduces the complexity of Nix-generated configs in favor of traditional dotfiles 
symlink management.

The ultimate goal is to drop Home Manager entirely and manage all configurations 
through direct symlinks and NixOS for package management only.

## Code Style Guidelines

### Nix Files

- When creating modules, prefer single file (`name.nix`) over directories (`./name/default.nix`)
- Format all files with `alejandra`
- Function parameters on separate lines with `}:`
- `let...in` blocks for local bindings
- Attribute sets use `{}` with proper alignment
- String interpolation: `"${variable}"`
- Comments with `#` prefix
- Import paths: `./relative/path.nix`

### Imports & Dependencies

- System imports: `{pkgs, lib, ...}:`
- Input references: `inputs."${name}"`
- Use `lib.` prefix for lib functions
- Overlays for custom packages/NURs

### Naming Conventions

- camelCase for variables and functions
- kebab-case for package names and attributes
- hostname-based configurations
- Module files: `default.nix`

### Error Handling

- Use `lib.mkIf` for conditionals
- Platform-specific with `pkgs.stdenv.isDarwin`
- Optional modules with `lib.optionals`
