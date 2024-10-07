{
  home-manager,
  pkgs,
  system ? "x86_64-linux",
  hostname,
  inputs,
}: let
  lib = pkgs.lib;

  isHeadless = hostname == "headless";

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
        stablePkgs = [
          # Match what NixOS is using
          "gnupg"
          "redis"
          "lua-language-server"
          # These are broken in unstable for one reason or another.
          "vim-language-server"
          # These need to be built on unstable in some cases, and
          # building them takes forever.
          "ffmpeg"
        ];
        useStablePkg = name: {
          ${name} = inputs.nixpkgs.legacyPackages.${self.system}.${name};
        };
      in
        lib.foldr lib.recursiveUpdate {} (map useStablePkg stablePkgs)
    )

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

    (
      self: super: {
        mdtree = inputs.carlos.packages.${self.system}.mdtree;
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
    lib.optionals (pkgs.stdenv.isLinux && ! isHeadless) [
      {
        home.packages = with pkgs; [
          _1password
          _1password-gui
          brave
          dunst
          eyedropper
          feh
          firefox
          inputs.ghostty.packages.${system}.default
          google-chrome
          gthumb
          shotgun
          slop
          tdesktop
          vlc
          xfce.thunar
        ];
      }
      ./discord.nix
      ./hyprland
      ./picom.nix
      ./rofi
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin
    [
      ./hammerspoon
    ];
in
  home-manager.lib.homeManagerConfiguration {
    pkgs = pkgs // {inherit overlays;};
    extraSpecialArgs = {
      inherit inputs system fonts charmPkgs carlosPkgs isHeadless;
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
        ./carapace.nix
        ./darwin-app-activation.nix
        ./emacs
        ./floskell
        ./fonts.nix
        ./fourmolu
        ./git.nix
        ./go.nix
        ./gpg.nix
        ./helix
        ./kakoune
        ./pkgs.nix
        ./prettier
        ./readline.nix
        ./ssh.nix
        ./scripts
        ./tmux
        ./wezterm
        ./vim
        ./zellij.nix
      ]
      ++ (lib.optionals (! isHeadless) [
        ./ghostty.nix
        ./kitty.nix
      ])
      ++ extraModules;
  }
