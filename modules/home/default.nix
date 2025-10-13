{
  home-manager,
  pkgs,
  system ? "x86_64-linux",
  inputs,
}: let
  lib = pkgs.lib;

  # 3rd party fonts
  fonts = [
    "anchor"
    "arno-pro"
    "benjamins-gothic"
    "gabriello"
    "gelion"
    "larsseit"
    "liza"
    "maru"
    "monoflow"
    "mononoki"
    "mori"
    "neufile-grotesk"
    "pangaia"
    "pique"
    "rifton"
    "rois"
    "saans"
    "sf-mono"
    "symbolset"
    "untitled-sans"
    "upton"
  ];

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

    # Custom fonts
    (self: super: let
      mkFont = name: {
        "${name}" = pkgs.stdenv.mkDerivation {
          inherit name;
          src = inputs."${name}";
          installPhase = ''
            mkdir -p $out/share/fonts/otf
            cp $src/* $out/share/fonts/otf
          '';
        };
      };
    in
      with lib; foldr recursiveUpdate {} (map mkFont fonts))
  ];
in
  home-manager.lib.homeManagerConfiguration {
    pkgs = pkgs // {inherit overlays;};
    extraSpecialArgs = {
      inherit inputs system fonts charmPkgs carlosPkgs;
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
      ./fonts.nix
      ./neovim
      ./pkgs.nix
    ];
  }
