{
  pkgs,
  lib,
  charmPkgs,
  ...
}: {
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
      gnupg
      gnused
      gnutar
      htop
      imagemagick
      jq
      librsvg # for the rsvg-convert binary
      libwebp
      lsd
      luajit
      moreutils
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
      tmux
      tree
      tree-sitter
      ttyd
      viu
      vscode
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
      pkgs.haskell.compiler.ghc948
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
    # X11 dev
    ++ (lib.optionals pkgs.stdenv.isLinux (with pkgs; [
      libGL
      xorg.libXcursor
      xorg.libXi
      xorg.libXinerama
      xorg.libXrandr
      xorg.libXxf86vm
    ]))
    # Linux
    ++ (lib.optionals pkgs.stdenv.isLinux (with pkgs; [
      element
    ]))
    # macOS
    ++ (lib.optionals pkgs.stdenv.isDarwin (with pkgs; [
      blender
      cachix
      dozer
      monitorcontrol
      nightfall
    ]))
    # Charm NUR
    ++ (map (x: pkgs.${x}) charmPkgs);

  programs = {
    z-lua.enable = true;
  };
}
