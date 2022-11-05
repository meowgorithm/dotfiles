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

    overlays = [];

    mkHome = {
      name,
      system,
      default,
      extraModules,
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
                  (
                    if isDarwin
                    then "/Users/"
                    else "/home/"
                  )
                  + home.username;
              }
              ./modules/bash
              ./modules/floskell
              ./modules/git.nix
              ./modules/gpg.nix
              ./modules/helix
              ./modules/kakoune
              ./modules/kitty.nix
              ./modules/pkgs.nix
              ./modules/readline
              ./modules/scripts
              ./modules/tmux
            ]
            ++ extraModules;
        };
      }
      // (
        if default
        then {packages."${system}".default = self.homeManagerConfigurations."${name}".activationPackage;}
        else {packages."${system}"."${name}" = self.homeManagerConfigurations."${name}".activationPackage;}
      );

    systems = let
      macOSModules = [
        ./modules/alacritty.nix
      ];
    in [
      rec {
        name = "linux";
        system = "x86_64-linux";
        default = true;
        extraModules = [
          {
            home.packages = with nixpkgs.legacyPackages."${system}"; [
              dunst
              feh
              firefox
              gthumb
              jetbrains-mono
              rofi
              shotgun
              slop
              xfce.thunar
            ];
          }
          ./modules/alacritty.nix
          ./modules/picom
          ./modules/x11
        ];
      }
      {
        name = "macOS-intel";
        system = "x86_64-darwin";
        default = true;
        extraModules = macOSModules;
      }
      {
        name = "macOS-arm";
        system = "aarch64-darwin";
        default = true;
        extraModules = macOSModules;
      }
      {
        name = "headless";
        system = "x86_64-linux";
        default = false;
        extraModules = [];
      }
    ];
  in
    lib.foldr lib.recursiveUpdate {} (map mkHome systems);
}
