{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "flake:nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    # macOS stuff
    mkAlias = {
      url = "github:cdmistman/mkAlias";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    blenderMacOSAarch64 = {
      url = "https://mirror.clarkson.edu/blender/release/Blender3.5/blender-3.5.1-macos-arm64.dmg";
      flake = false;
    };
    blenderMacOSx86_64 = {
      url = "https://mirror.clarkson.edu/blender/release/Blender3.5/blender-3.5.1-macos-x64.dmg";
      flake = false;
    };
    dozer = {
      url = "https://github.com/Mortennn/Dozer/releases/download/v4.0.0/Dozer.4.0.0.dmg";
      flake = false;
    };
    element = {
      url = "https://packages.riot.im/desktop/install/macos/Element.dmg";
      flake = false;
    };
    nightfall = {
      url = "https://github.com/r-thomson/Nightfall/releases/download/v3.0.0/Nightfall.dmg";
      flake = false;
    };
    telegramMacOS = {
      url = "https://telegram.org/dl/macos";
      flake = false;
    };

    # Hammerspoon
    hammerspoon = {
      url = "https://github.com/Hammerspoon/hammerspoon/releases/download/0.9.100/Hammerspoon-0.9.100.zip";
      flake = false;
    };
    spoonInstall = {
      url = "https://raw.githubusercontent.com/Hammerspoon/Spoons/1438f747d4a49932a1d2c4911eb05c30b785fb49/Source/SpoonInstall.spoon/init.lua";
      flake = false;
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
    gelion = {
      url = "git+ssh://git.rocha.is/gelion";
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
    neufile-grotesk = {
      url = "git+ssh://git.rocha.is/neufile-grotesk";
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
          modules = [
            ({
              pkgs,
              modulesPath,
              ...
            }: {
              system.configurationRevision = lib.mkIf (self ? rev) self.rev;
            })
            (import ./modules/nixos hostname)
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
