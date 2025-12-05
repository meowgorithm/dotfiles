{
  inputs.nixpkgs.url = "flake:nixpkgs";

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
            inherit system home-manager inputs;
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
        hostname = "artemis";
        system = x86_64-linux;
        default = true;
      }
      {
        # NixOS Desktop
        hostname = "whitenoise";
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
      }
    ]);
}
