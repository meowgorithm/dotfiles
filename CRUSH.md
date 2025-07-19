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

## Code Style Guidelines

### Nix Files

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

