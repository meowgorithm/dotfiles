---
name: nix-profile
description: Manage Nix profiles using the modern `nix profile` command (experimental, replaces `nix-env`). Use when installing, removing, upgrading, or managing packages with Nix flakes.
---

# Nix Profile

Manage Nix profiles using the modern `nix profile` command (experimental,
replaces `nix-env`).

## Overview

`nix profile` is the modern, flake-native way to manage Nix profiles. It
replaces the older `nix-env` command with a more intuitive interface built
around flakes.

**Warning**: Once you use `nix profile`, you cannot use `nix-env` on the same
profile without first deleting it.

## Profile Location

Profiles are stored at:

- `$XDG_STATE_HOME/nix/profiles/profile` (default for regular users)
- `$NIX_STATE_DIR/profiles/per-user/root/profile` (for root)

The active profile is symlinked at:

- `~/.nix-profile` (traditional)
- `$XDG_STATE_HOME/nix/profile` (if `use-xdg-base-directories` is enabled)

## Commands

### Install a package

```bash
nix profile add nixpkgs#hello
nix profile add nixpkgs/release-23.11#hello
nix profile add nixpkgs/d73407e8e6002646acfdef0e39ace088bacc83da#hello
nix profile add nixpkgs#bash^man          # specific output
```

Alias: `nix profile install` (same as `add`)

### List installed packages

```bash
nix profile list
```

Shows:

- Name (used for remove/upgrade)
- Flake attribute path
- Original and locked flake URLs
- Store paths

### Remove packages

```bash
nix profile remove hello                  # by name
nix profile remove --all                  # remove everything
nix profile remove --regex '.*vim.*'      # by regex
nix profile remove /nix/store/...-hello   # by store path
```

### Upgrade packages

```bash
nix profile upgrade hello                 # upgrade specific package
nix profile upgrade --all                 # upgrade everything
nix profile upgrade --regex '.*vim.*'     # upgrade matching packages
```

Note: Only works with unlocked flake references (e.g., `nixpkgs#hello`). Locked references (full git rev) won't upgrade.

### View profile history

```bash
nix profile history
```

Shows changes between profile versions (additions, removals, upgrades).

### Roll back changes

```bash
nix profile rollback                      # roll back to previous version
nix profile rollback --to 510             # roll back to specific version
```

### Show closure differences

```bash
nix profile diff-closures
```

Shows detailed differences between profile versions including all dependencies.

### Clean up old versions

```bash
nix profile wipe-history                  # delete all non-current versions
nix profile wipe-history --older-than 30d # delete versions older than 30 days
nix profile wipe-history --dry-run        # preview what would be deleted
```

## Common Options

- `--profile /path/to/profile` - operate on a custom profile path
- `--priority N` - set package priority (lower = higher priority)
- `--dry-run` - show what would happen without making changes

## Migration from nix-env

If you need to go back to `nix-env`:

```bash
# Warning: this deletes all installed packages
rm -rf "${XDG_STATE_HOME:-$HOME/.local/state}/nix/profiles/profile"
```

## Key Differences from nix-env

| Feature         | nix-env                     | nix profile                     |
| --------------- | --------------------------- | ------------------------------- |
| Flake-native    | No                          | Yes                             |
| Profile format  | manifest.nix                | manifest.json                   |
| Install command | `nix-env -iA nixpkgs.hello` | `nix profile add nixpkgs#hello` |
| Remove by name  | `nix-env -e hello`          | `nix profile remove hello`      |
| Upgrade         | `nix-env -u`                | `nix profile upgrade --all`     |
| List            | `nix-env -q`                | `nix profile list`              |

## Filesystem Layout

```
~/.local/state/nix/profiles/
├── profile -> profile-7-link                    # current profile symlink
├── profile-5-link -> /nix/store/...-profile     # old version
├── profile-6-link -> /nix/store/...-profile     # old version
└── profile-7-link -> /nix/store/...-profile     # current version
```

Each profile version contains:

- `bin/` - symlinks to installed package binaries
- `share/` - shared files
- `manifest.json` - profile manifest
