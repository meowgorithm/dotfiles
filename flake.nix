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
      isDarwin = pkgs.stdenv.isDarwin;
    in {
      homeManagerConfigurations."${system}" = homeManager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules =
          [
            rec {
              home.stateVersion = "22.11";
              home.username = "christian";
              home.homeDirectory =
                if pkgs.stdenv.isDarwin
                then "/Users/"
                else "/home/" + home.username;
            }
            ./modules/bash
            ./modules/floskell
            ./modules/git.nix
            ./modules/pkgs.nix
            ./modules/readline
            ./modules/tmux
          ]
          ++ (
            if isDarwin
            then []
            else [./modules/alacritty.nix]
          );
      };

      packages."${system}".default = self.homeManagerConfigurations."${system}".activationPackage;
    };

    systems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
  in
    lib.foldr lib.recursiveUpdate {} (map mkHome systems);
}
