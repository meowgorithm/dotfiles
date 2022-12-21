{
  home-manager,
  pkgs,
  system ? "x86_64-linux",
  hostname,
  helix,
}: let
  helixPackage =
    if hostname == ""
    then pkgs.helix
    else helix.packages.${system}.default;

  overlays = [
    (import ./fonts/external.nix)
  ];

  extraModules =
    (
      if pkgs.stdenv.isLinux && hostname != "headless"
      then [
        {
          home.packages = with pkgs; [
            brave
            dunst
            feh
            firefox
            gcolor2
            gthumb
            shotgun
            slop
            xfce.thunar
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
        ./fonts
        ./git.nix
        ./gpg.nix
        ./kakoune
        ./kitty.nix
        ./pkgs.nix
        ./readline
        ./scripts
        ./tmux
      ]
      ++ extraModules;
  }
