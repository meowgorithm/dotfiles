{
  home-manager,
  pkgs,
  system ? "x86_64-linux",
  inputs,
  headless,
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

    # macOS applications
    (
      self: super: {
        dozer = mkDmg "dozer" "Dozer" inputs.dozer;
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

  extraModules = lib.optionals (pkgs.stdenv.isLinux && ! headless) [
    {
      home.packages = with pkgs; [
        _1password-cli
        _1password-gui
        brave
        dunst
        eyedropper
        feh
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
    ./firefox.nix
    ./hyprland
    ./picom.nix
    ./rofi
    ./waybar.nix
  ];
in
  home-manager.lib.homeManagerConfiguration {
    pkgs = pkgs // {inherit overlays;};
    extraSpecialArgs = {
      inherit inputs system fonts charmPkgs carlosPkgs headless;
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
        ./bash
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
        ./neovim
        ./nushell.nix
        ./pkgs.nix
        ./prettier
        ./readline.nix
        ./scripts
        ./ssh.nix
        ./tmux
        ./wezterm
      ]
      ++ (lib.optionals (! headless) [
        ./alacritty.nix
        ./ghostty.nix
        ./kitty.nix
        ./rio.nix
      ])
      ++ extraModules;
  }
