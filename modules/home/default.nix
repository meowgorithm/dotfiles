{
  home-manager,
  pkgs,
  system ? "x86_64-linux",
  hostname,
  inputs,
}: let
  lib = pkgs.lib;

  helixPackage =
    if hostname == ""
    then pkgs.helix
    else inputs.helix.packages.${system}.default;

  mkFont = vals: let
    name = lib.elemAt vals 0;
    pkg = lib.elemAt vals 1;
  in {
    "${name}" = pkgs.stdenv.mkDerivation {
      inherit name;
      src = pkg;
      installPhase = ''
        mkdir -p $out/share/fonts/otf
        cp $src/* $out/share/fonts/otf
      '';
    };
  };

  overlays = [
    (
      self: super: let
        stablePkgs = inputs.nixpkgs.legacyPackages.${system};
      in {
        kitty = stablePkgs.kitty;
        gnupg = stablePkgs.gnupg;
      }
    )
    (
      self: super:
        lib.foldr lib.recursiveUpdate {}
        (map mkFont (with inputs; [
          ["anchor" anchor]
          ["arno-pro" arno-pro]
          ["benjamins-gothic" benjamins-gothic]
          ["gabriello" gabriello]
          ["larsseit" larsseit]
          ["monoflow" monoflow]
          ["pique" pique]
          ["rifton" rifton]
          ["rois" rois]
          ["sf-mono" sf-mono]
          ["symbolset" symbolset]
          ["untitled-sans" untitled-sans]
          ["upton" upton]
        ]))
    )
  ];

  extraModules =
    (
      if pkgs.stdenv.isLinux && hostname != "headless"
      then [
        {
          home.packages = with pkgs; [
            _1password
            _1password-gui
            brave
            dunst
            feh
            firefox
            gcolor2
            google-chrome
            gthumb
            shotgun
            slop
            tdesktop
            vlc
            xclip
            xfce.thunar
            xsel
          ];
        }
        ./alacritty.nix
        ./discord.nix
        ./picom.nix
        ./rofi
        ./x11
        ./xmonad
      ]
      else []
    )
    ++ (
      if pkgs.stdenv.isDarwin
      then [
        ./alacritty.nix
      ]
      else []
    );
in
  home-manager.lib.homeManagerConfiguration {
    pkgs = pkgs // {inherit overlays;};
    modules =
      [
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
        (import ./helix helixPackage)
        ./bash
        ./floskell
        ./fonts.nix
        ./git.nix
        ./gpg.nix
        ./kakoune
        ./kitty.nix
        ./pkgs.nix
        ./readline.nix
        ./scripts
        ./tmux
      ]
      ++ extraModules;
  }
