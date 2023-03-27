{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "flake:nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    helix.url = "github:helix-editor/helix/master";
    mkAlias = {
      url = "github:cdmistman/mkAlias";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    # Fonts
    anchor = {
      url = "git+ssh://git.rocha.is/anchor";
      flake = false;
    };
    arno-pro = {
      url = "git+ssh://git.rocha.is/arno-pro";
      flake = false;
    };
    benjamins-gothic = {
      url = "git+ssh://git.rocha.is/benjamins-gothic";
      flake = false;
    };
    gabriello = {
      url = "git+ssh://git.rocha.is/gabriello";
      flake = false;
    };
    larsseit = {
      url = "git+ssh://git.rocha.is/larsseit";
      flake = false;
    };
    monoflow = {
      url = "git+ssh://git.rocha.is/monoflow";
      flake = false;
    };
    pique = {
      url = "git+ssh://git.rocha.is/pique";
      flake = false;
    };
    rifton = {
      url = "git+ssh://git.rocha.is/rifton";
      flake = false;
    };
    rois = {
      url = "git+ssh://git.rocha.is/rois";
      flake = false;
    };
    sf-mono = {
      url = "git+ssh://git.rocha.is/sf-mono";
      flake = false;
    };
    space-grotesk = {
      url = "git+ssh://git.rocha.is/space-grotesk";
      flake = false;
    };
    symbolset = {
      url = "git+ssh://git.rocha.is/symbolset";
      flake = false;
    };
    untitled-sans = {
      url = "git+ssh://git.rocha.is/untitled-sans";
      flake = false;
    };
    upton = {
      url = "git+ssh://git.rocha.is/upton";
      flake = false;
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    ...
  }: let
    lib = nixpkgs.lib;
    x86_64-linux = "x86_64-linux";
    x86_64-darwin = "x86_64-darwin";
    aarch64-darwin = "aarch64-darwin";

    mkSystem = {
      hostname,
      system ? x86_64-linux,
      default ? false,
    }:
      {
        nixosConfigurations."${hostname}" = lib.nixosSystem {
          inherit system;
          extraSpecialArgs = {
            inherit hostname;
          };
          modules = [
            ({
              pkgs,
              modulesPath,
              ...
            }: {
              system.configurationRevision = lib.mkIf (self ? rev) self.rev;
            })
            ./modules/nixos
          ];
        };

        homeManagerConfigurations."christian@${hostname}" =
          import ./modules/home
          {
            pkgs = import nixpkgs-unstable {
              inherit system;
              config.allowUnfree = true;
            };
            inherit system home-manager hostname inputs;
          };
      }
      // (
        let
          target =
            if default
            then "default"
            else "christian@${hostname}";
        in {
          packages."${system}"."${target}" = self.homeManagerConfigurations."christian@${hostname}".activationPackage;
        }
      );
  in
    lib.foldr lib.recursiveUpdate {} (map mkSystem [
      {
        hostname = "stardust";
        system = x86_64-linux;
        default = true;
      }
      {
        hostname = "purrmachine";
        system = x86_64-linux;
      }
      {
        hostname = "headless";
        system = x86_64-linux;
      }
      {
        hostname = "thunderclap";
        system = x86_64-darwin;
        default = true;
      }
      {
        hostname = "meowmachine";
        system = aarch64-darwin;
        default = true;
      }
    ]);
}
