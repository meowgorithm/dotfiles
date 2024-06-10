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
      bashInteractive
      bat
      bc
      brotli
      cargo
      coreutils
      ctags
      curl
      clang
      direnv
      doctl
      duf
      elvish
      ffmpeg
      fish
      faketty
      fzf
      getopt
      gh
      gnupg
      gnused
      gnutar
      htop
      imagemagick
      jq
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
      # librsvg # for the rsvg-convert binary
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
    # Rust
    ++ (with pkgs; [
      rust-analyzer
      rustc
    ])
    # Haskell
    ++ (with pkgs.haskellPackages; [
      cabal-fmt
      cabal-install
      pkgs.haskell.compiler.ghc948
      haskell-language-server
    ])
    # LSP
    ++ (with pkgs;
      with pkgs.elmPackages;
      with pkgs.nodePackages_latest; [
        alejandra
        bash-language-server
        # elm
        elm-language-server
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
