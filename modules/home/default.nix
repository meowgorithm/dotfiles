{
  home-manager,
  pkgs,
  system ? "x86_64-linux",
  hostname,
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
    "monoflow"
    "mononoki"
    "mori"
    "neufile-grotesk"
    "pangaia"
    "pique"
    "rifton"
    "rois"
    "sf-mono"
    "symbolset"
    "untitled-sans"
    "upton"
  ];

  # Packges from the Charm Nix User Respository
  charmPkgs = [
    "gum"
    "freeze"
    "melt"
    "mods"
    "pop"
    "soft-serve"
    "vhs"
    "wishlist"
  ];

  # Build a macOS application from a DMG. Will do nothing if the OS is not
  # macOS.
  mkDmg = name: appName: src: let
    mkIfDarwin = pkgs.lib.mkIf pkgs.stdenv.isDarwin;
  in
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
    # Use stable packages to match what NixOS is using
    (
      self: super: let
        stablePkgs = ["gnupg" "redis"];
        useStablePkg = name: {
          ${name} = inputs.nixpkgs.legacyPackages.${self.system}.${name};
        };
      in
        lib.foldr lib.recursiveUpdate {} (map useStablePkg stablePkgs)
    )

    # Add packages from Charm NUR
    (
      self: super: let
        useCharmPkg = name: {
          ${name} = inputs.charm.legacyPackages.${self.system}.${name};
        };
      in
        lib.foldr lib.recursiveUpdate {} (map useCharmPkg charmPkgs)
    )

    # Helix bleeding edge
    (
      self: super: {
        helix = inputs.helix.packages.${self.system}.default;
      }
    )

    # macOS applications
    (
      self: super: {
        blender = mkDmg "blender" "Blender" (
          if self.pkgs.stdenv.hostPlatform.system == "aarch64-darwin"
          then inputs.blenderMacOSAarch64
          else inputs.blenderMacOSx86_64
        );
        dozer = mkDmg "dozer" "Dozer" inputs.dozer;
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
          inputs.ghostty.packages.${system}.default
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
      inherit inputs system fonts charmPkgs;
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
