{
  pkgs,
  lib,
  charmPkgs,
  carlosPkgs,
  headless,
  ...
}: let
  isLinux = pkgs.stdenv.isLinux;
  isDarwin = pkgs.stdenv.isDarwin;
in {
  home.packages = with pkgs;
    [
      age
      awscli2
      bat
      bc
      brotli
      cargo
      clang
      coreutils
      ctags
      curl
      direnv
      doctl
      duf
      elvish
      faketty
      ffmpeg
      fish
      fzf
      getopt
      gh
      gnumake
      gnupg
      gnused
      gnutar
      htop
      imagemagick
      jq
      librsvg # for the rsvg-convert binary
      libwebp
      luajit
      moreutils
      mosh
      ngrok
      nodejs
      nushell
      optipng
      pastel
      postgresql
      python3
      redis
      ripgrep
      shellcheck
      shfmt
      simple-http-server
      tree
      tree-sitter
      ttyd
      viu
      wget
      yq
      yubikey-manager
      z-lua
      zopfli
    ]
    # Haskell
    ++ (with pkgs.haskellPackages; [
      cabal-fmt
      cabal-install
      pkgs.haskell.compiler.ghc982
      haskell-language-server
    ])
    # Elm
    ++ (with pkgs.elmPackages; [
      elm
      elm-language-server
    ])
    # Rust
    ++ (with pkgs; [
      rust-analyzer
      rustc
    ])
    # Various LSP
    ++ (with pkgs;
      with pkgs.nodePackages_latest; [
        alejandra
        bash-language-server
        golangci-lint-langserver
        lua-language-server
        nil
        prettier
        semgrep
        stylua
        svgo
        typescript
        typescript-language-server
        vim-language-server
        vscode-langservers-extracted
        yaml-language-server
      ])
    # Desktops only
    ++ (
      lib.optionals (! headless) (with pkgs; [
        vscode
      ])
    )
    # X11 dev
    ++ (lib.optionals (isLinux && ! headless) (with pkgs; [
      libGL
      xorg.libXcursor
      xorg.libXi
      xorg.libXinerama
      xorg.libXrandr
      xorg.libXxf86vm
    ]))
    # Linux Desktop
    ++ (lib.optionals (isLinux && ! headless) (with pkgs; [
      element
    ]))
    # macOS
    ++ (lib.optionals isDarwin (with pkgs; [
      blender
      cachix
      dozer
      monitorcontrol
    ]))
    # Charm NUR
    ++ (map (x: pkgs.${x}) charmPkgs)
    # Carlos' NUR
    ++ (map (x: pkgs.${x}) carlosPkgs);

  programs = {
    z-lua.enable = true;
  };
}
