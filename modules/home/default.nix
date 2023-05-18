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
    # Use the same version as the system
    (
      self: super: let
        stablePkgs = inputs.nixpkgs.legacyPackages.${system};
      in {
        gnupg = stablePkgs.gnupg;
      }
    )
    # macOS stuff
    (
      self: super: {
        blender = mkDmg "blender" "Blender" (
          if pkgs.stdenv.hostPlatform.system == "aarch64-darwin"
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
        telegram = mkDmg "telegram" "Telegram" inputs.telegramMacOS;
      }
    )
    # Custom fonts
    (
      self: super:
        lib.foldr lib.recursiveUpdate {}
        (map mkFont fonts)
    )
    # Vim plugins
    (self: super: {
      vim-colortemplate = self.vimUtils.buildVimPluginFrom2Nix {
        name = "vim-colortemplate";
        src = inputs.vimColorTemplate;
      };
    })
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
      ./alacritty.nix
      ./discord.nix
      ./picom.nix
      ./rofi
      ./x11
      ./xmonad
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin
    [
      ./alacritty.nix
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
        ./bash
        ./darwin-app-activation.nix
        ./floskell
        ./fonts.nix
        ./git.nix
        ./gpg.nix
        ./helix
        ./kakoune
        ./kitty.nix
        ./pkgs.nix
        ./readline.nix
        ./scripts
        ./tmux
        ./vim
      ]
      ++ extraModules;
  }
