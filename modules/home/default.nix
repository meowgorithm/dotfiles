{
  home-manager,
  pkgs,
  system ? "x86_64-linux",
  hostname,
  inputs,
}: let
  lib = pkgs.lib;

  fonts = [
    "anchor"
    "arno-pro"
    "benjamins-gothic"
    "gabriello"
    "gelion"
    "larsseit"
    "monoflow"
    "neufile-grotesk"
    "pique"
    "rifton"
    "rois"
    "sf-mono"
    "symbolset"
    "untitled-sans"
    "upton"
  ];

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

  mkIfDarwin = pkgs.lib.mkIf pkgs.stdenv.isDarwin;

  mkDmg = name: appName: src:
    mkIfDarwin (pkgs.stdenv.mkDerivation {
      inherit name src;
      buildInputs = with pkgs; [undmg];
      sourceRoot = "${appName}.app";
      phases = ["unpackPhase" "installPhase"];
      unpackPhase = ''
        undmg "${src}";
      '';
      installPhase = ''
        mkdir -p "$out/Applications/${appName}.app"
        cp -pR * "$out/Applications/${appName}.app"
      '';
    });

  overlays = [
    (
      self: super: let
        stablePkgs = inputs.nixpkgs.legacyPackages.${system};
      in {
        # Use the same version as the system
        gnupg = stablePkgs.gnupg;
        redis = stablePkgs.redis;

        # Unstable currently broken on macOS (x86_64 and/or aarch64)
        vscode-langservers-extracted = stablePkgs.nodePackages_latest.vscode-langservers-extracted;
        lua-language-server = stablePkgs.lua-language-server;
        awscli2 = stablePkgs.awscli2;
        ffmpeg = stablePkgs.ffmpeg;
      }
    )

    # Charm
    (
      self: super: let
        charmPkgs = [
          "gum"
          "melt"
          "mods"
          "pop"
          "soft-serve"
          "vhs"
          "wishlist"
        ];
        useCharmPkg = name: {
          "${name}" = inputs.charm.legacyPackages.${system}."${name}";
        };
      in
        lib.foldr lib.recursiveUpdate {} (map useCharmPkg charmPkgs)
    )

    # macOS stuff
    (
      self: super: {
        blender = mkDmg "blender" "Blender" (
          if self.pkgs.stdenv.hostPlatform.system == "aarch64-darwin"
          then inputs.blenderMacOSAarch64
          else inputs.blenderMacOSx86_64
        );
        dozer = mkDmg "dozer" "Dozer" inputs.dozer;
        element = mkDmg "element" "Element" inputs.element;
        hammerspoon = self.pkgs.stdenv.mkDerivation {
          name = "hammerspoon";
          src = inputs.hammerspoon;
          installPhase = ''
            mkdir -p "$out/Applications/Hammerspoon.app";
            cp -r "$src/Contents" "$out/Applications/Hammerspoon.app";
          '';
        };
        nightfall = mkDmg "nightfall" "Nightfall" inputs.nightfall;
        monitorcontrol = mkDmg "monitorcontrol" "MonitorControl" inputs.monitorcontrol;
      }
    )

    # Custom fonts
    (
      self: super:
        with lib;
          foldr recursiveUpdate {} (map mkFont fonts)
    )
  ];

  extraModules =
    lib.optionals (pkgs.stdenv.isLinux && hostname != "headless") [
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
      ./discord.nix
      ./picom.nix
      ./rofi
      ./x11
      ./xmonad
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin
    [
      ./hammerspoon
    ];
in
  home-manager.lib.homeManagerConfiguration {
    pkgs = pkgs // {inherit overlays;};
    extraSpecialArgs = {
      inherit inputs system fonts;
    };
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
        ./alacritty.nix
        ./bash
        ./darwin-app-activation.nix
        ./emacs
        ./floskell
        ./fonts.nix
        ./fourmolu
        ./ghostty.nix
        ./git.nix
        ./go.nix
        ./gpg.nix
        ./helix
        ./kakoune
        ./kitty.nix
        ./pkgs.nix
        ./prettier
        ./readline.nix
        ./scripts
        ./tmux
        ./wezterm
        ./vim
        ./zellij.nix
      ]
      ++ extraModules;
  }
