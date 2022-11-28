{
  home-manager,
  pkgs,
  system ? "x86_64-linux",
  hostname,
}: let
  extraModules =
    (
      if pkgs.stdenv.isLinux && hostname != "headless"
      then [
        {
          home.packages = with pkgs; [
            dunst
            feh
            firefox
            gthumb
            rofi
            shotgun
            slop
            xfce.thunar
          ];
        }
        ./alacritty.nix
        ./discord.nix
        ./picom
        ./x11
        ./xmonad
      ]
      else []
    )
    ++ (
      if pkgs.stdenv.isDarwin
      then [
        ./modules/alacritty.nix
      ]
      else []
    );
in
  home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
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
        ./bash
        ./floskell
        ./git.nix
        ./gpg.nix
        ./helix
        ./kakoune
        ./kitty.nix
        ./pkgs.nix
        ./readline
        ./scripts
        ./tmux
      ]
      ++ extraModules;
  }
