{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "flake:nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    helix.url = "github:helix-editor/helix/master";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    helix,
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

        homeManagerConfigurations."christian@${hostname}" = let
          pkgs = nixpkgs-unstable.legacyPackages."${system}";
        in (import ./modules/home
          {
            inherit pkgs;
            inherit system;
            inherit home-manager;
            inherit hostname;
            inherit helix;
          });
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
