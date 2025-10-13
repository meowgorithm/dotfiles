{
  home-manager,
  pkgs,
  system ? "x86_64-linux",
  inputs,
}: let
  lib = pkgs.lib;

  # Packges from the Charm Nix User Respository
  charmPkgs = [
    "freeze"
    "glow"
    "gum"
    "melt"
    "mods"
    "pop"
    "skate"
    "soft-serve"
    "vhs"
    "wishlist"
  ];

  # Packages from Carlos' Nix User Repository
  carlosPkgs = [
    "mdtree"
    "svu"
  ];

  overlays = [
    # Add packages from NURs
    (
      self: super: let
        useNurPkg = nurName: pkgName: {
          ${pkgName} = inputs.${nurName}.packages.${self.system}.${pkgName};
        };
      in
        lib.foldr lib.recursiveUpdate {} (
          (map (useNurPkg "charm") charmPkgs)
          ++ (map (useNurPkg "carlos") carlosPkgs)
        )
    )
  ];
in
  home-manager.lib.homeManagerConfiguration {
    pkgs = pkgs // {inherit overlays;};
    extraSpecialArgs = {
      inherit inputs system charmPkgs carlosPkgs;
    };
    modules = [
      rec {
        home.stateVersion = "22.11";
        home.username = "christian";
        home.homeDirectory =
          (
            if pkgs.stdenv.isDarwin
            then "/Users/"
            else "/home/"
          )
          + home.username;
      }
      ./neovim
      ./pkgs.nix
    ];
  }
