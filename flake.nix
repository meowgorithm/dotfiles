{
  inputs.nixpkgs.url = "flake:nixpkgs";
  inputs.home-manager.url = "github:nix-community/home-manager";
  # inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
  inputs.charm.url = "github:charmbracelet/nur";
  inputs.charm.inputs.nixpkgs.follows = "nixpkgs";
  inputs.carlos.url = "github:caarlos0/nur";
  inputs.carlos.inputs.nixpkgs.follows = "nixpkgs";

  # Fonts
  inputs.anchor.url = "git+ssh://git.rocha.is/anchor";
  inputs.anchor.flake = false;
  inputs.arno-pro.url = "git+ssh://git.rocha.is/arno-pro";
  inputs.arno-pro.flake = false;
  inputs.benjamins-gothic.url = "git+ssh://git.rocha.is/benjamins-gothic";
  inputs.benjamins-gothic.flake = false;
  inputs.gabriello.url = "git+ssh://git.rocha.is/gabriello";
  inputs.gabriello.flake = false;
  inputs.gelion.url = "git+ssh://git.rocha.is/gelion";
  inputs.gelion.flake = false;
  inputs.larsseit.url = "git+ssh://git.rocha.is/larsseit";
  inputs.larsseit.flake = false;
  inputs.liza.url = "git+ssh://git.rocha.is/liza";
  inputs.liza.flake = false;
  inputs.maru.url = "git+ssh://git.rocha.is/maru";
  inputs.maru.flake = false;
  inputs.monoflow.url = "git+ssh://git.rocha.is/monoflow";
  inputs.mononoki.url = "git+ssh://git.rocha.is/mononoki";
  inputs.mononoki.flake = false;
  inputs.mori.url = "git+ssh://git.rocha.is/mori";
  inputs.mori.flake = false;
  inputs.monoflow.flake = false;
  inputs.neufile-grotesk.url = "git+ssh://git.rocha.is/neufile-grotesk";
  inputs.neufile-grotesk.flake = false;
  inputs.pique.url = "git+ssh://git.rocha.is/pique";
  inputs.pique.flake = false;
  inputs.pangaia.url = "git+ssh://git.rocha.is/pangaia";
  inputs.pangaia.flake = false;
  inputs.rifton.url = "git+ssh://git.rocha.is/rifton";
  inputs.rifton.flake = false;
  inputs.rois.url = "git+ssh://git.rocha.is/rois";
  inputs.rois.flake = false;
  inputs.sf-mono.url = "git+ssh://git.rocha.is/sf-mono";
  inputs.sf-mono.flake = false;
  inputs.saans.url = "git+ssh://git.rocha.is/saans";
  inputs.saans.flake = false;
  inputs.symbolset.url = "git+ssh://git.rocha.is/symbolset";
  inputs.symbolset.flake = false;
  inputs.untitled-sans.url = "git+ssh://git.rocha.is/untitled-sans";
  inputs.untitled-sans.flake = false;
  inputs.upton.url = "git+ssh://git.rocha.is/upton";
  inputs.upton.flake = false;

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    ...
  }: let
    lib = nixpkgs.lib;

    x86_64-linux = "x86_64-linux";
    aarch64-darwin = "aarch64-darwin";

    mkSystem = {
      hostname,
      system ? x86_64-linux,
      user ? "christian",
      default ? false,
      headless ? false,
    }:
      {
        nixosConfigurations."${hostname}" = lib.nixosSystem {
          inherit system;
          modules = [
            {
              nixpkgs.overlays = [
                (self: super: {
                  # Enable CUDA/Optix in Blender
                  blender = super.blender.override {cudaSupport = true;};
                })
              ];
            }
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
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };
            inherit system home-manager inputs headless;
          };
      }
      // (
        let
          target =
            if default
            then "default"
            else "${user}@${hostname}";
        in {
          packages."${system}"."${target}" = self.homeManagerConfigurations."${user}@${hostname}".activationPackage;
        }
      );
  in
    lib.foldr lib.recursiveUpdate {} (map mkSystem [
      {
        # NixOS Desktop
        hostname = "el-gato";
        system = x86_64-linux;
        default = true;
      }
      {
        # NixOS Desktop
        hostname = "athena";
        system = x86_64-linux;
      }
      {
        # Mac Studio
        hostname = "pantera";
        system = aarch64-darwin;
        default = true;
      }
      {
        # MacBook Pro
        hostname = "meowmachine";
        system = aarch64-darwin;
        default = true;
      }
      {
        # MacBook Air
        hostname = "la-tigra";
        system = aarch64-darwin;
      }
      {
        # WSL
        hostname = "wsl";
        system = x86_64-linux;
        user = "chris";
        headless = true;
      }
    ]);
}
