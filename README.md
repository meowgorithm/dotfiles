# The Meowgorithm Dotfiles, Collectors’ Edition

This repo contains a NixOS and Home Manager configurations to support a few
systems:

- NixOS x86_64
- macOS x86_64 and aarch64
- Headless, general-purpose Linux x86_64

## User Management

```bash
# Enable flakes on non-NixOS systems:
./enable-flakes

# Let it rip:
nix run

# Or, for headless Linux boxes:
nix run .#christian@headless
```

## OS Management

Update the system locally:

```bash
nixos-rebuild switch --flake .
````

Or just do it over the network:

```bash
nixos-rebuild switch --flake github:meowgorithm/dotfiles/master
```
