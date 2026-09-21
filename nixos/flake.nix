# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  description = "System Configuration";

  # Applies at evaluation time, so even the first rebuild on a fresh
  # machine substitutes from the cache (nix.settings in configuration.nix
  # only takes effect after activation).
  nixConfig = {
    extra-substituters = ["https://noctalia.cachix.org"];
    extra-trusted-public-keys = ["noctalia.cachix.org-1:pCOR47nnMEo5thcxNDtzWpOxNFQsBRglJzxWPp3dkU4="];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    noctalia = {
      url = "github:noctalia-dev/noctalia";
    };
    umbriel = {
      url = "github:noctalia-dev/umbriel";
    };
    noctalia-greeter = {
      url = "github:noctalia-dev/noctalia-greeter";
      # No nixpkgs.follows: keeping the repo's own pin produces the same
      # store paths CI pushes to noctalia.cachix.org, so builds are cached.
    };
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: {
    nixosConfigurations = {
      baobao = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {
          inherit inputs;
          hostname = "baobao";
        };
        modules = [
          ./configuration.nix
          ./discord.nix
          ./podman.nix
          ./zoom.nix
          ./gz302ea.nix
          ./baobao-hardware-configuration.nix
        ];
      };

      whitenoise = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {
          inherit inputs;
          hostname = "whitenoise";
        };
        modules = [
          ./configuration.nix
          ./discord.nix
          ./podman.nix
          ./zoom.nix
          ./rtx3090.nix
          ./whitenoise-hardware-configuration.nix
        ];
      };
    };
  };
}
