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

    mkHome = {
      name,
      system,
      default,
    }: let
      pkgs = inputs.nixpkgs.legacyPackages."${system}";
      isDarwin = pkgs.stdenv.isDarwin;
    in
      {
        homeManagerConfigurations."${name}" = homeManager.lib.homeManagerConfiguration {
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
              if name != "headless"
              then [./modules/alacritty.nix]
              else []
            );
        };
      }
      // (
        if default
        then {packages."${system}".default = self.homeManagerConfigurations."${name}".activationPackage;}
        else {}
      );

    systems = [
      {
        name = "linux";
        system = "x86_64-linux";
        default = true;
      }
      {
        name = "macOS-intel";
        system = "x86_64-darwin";
        default = true;
      }
      {
        name = "macOS-arm";
        system = "aarch64-darwin";
        default = true;
      }
      {
        name = "headless";
        system = "x86_64-linux";
        default = false;
      }
    ];
  in
    lib.foldr lib.recursiveUpdate {} (map mkHome systems);
}
