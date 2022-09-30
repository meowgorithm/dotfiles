# The Meowgorithm Dotfiles, Collectors’ Edition

This repo contains a Nix/Home Manager configuration to support a few systems:

- NixOS x86_64
- macOS x86_64 and aarch64
- Headless general-purpose Linux x86_64

```bash
# Enable flakes on non-NixOS systems:
./enable-flakes

# Let it rip:
nix run

# Or, for headless Linux boxes:
nix run .#headless
```
