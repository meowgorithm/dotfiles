{
  description = "The Panther Room";

  inputs = {
    nixpkgs.url = "flake:nixpkgs";
    homeManager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    homeManager,
  } @ inputs: let
    lib = nixpkgs.lib;

    mkHome = system: let
      pkgs = inputs.nixpkgs.legacyPackages."${system}";
      homeDir =
        (
          if pkgs.stdenv.isDarwin
          then "/Users/"
          else "/home/"
        )
        + "christian";
    in {
      homeManagerConfigurations."${system}" = homeManager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          {
            home = {
              username = "christian";
              homeDirectory = homeDir;
              stateVersion = "22.11";
            };
            programs = {
              home-manager.enable = true;
              bash = (import ./bash.nix) pkgs;
              alacritty = (import ./alacritty.nix) pkgs;
            };
          }

          ./pkgs.nix
          ./readline.nix
          ./git.nix
        ];
      };

      packages."${system}".default = self.homeManagerConfigurations."${system}".activationPackage;
    };

    systems = [
      "aarch64-darwin"
      "x86_64-darwin"
    ];
  in
    lib.foldr lib.recursiveUpdate {} (map mkHome systems);
}
