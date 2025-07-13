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
  # isX86_64 = pkgs.stdenv.isx86_64;
in {
  home.packages = with pkgs;
    [
      age
      awscli2
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
      go-task
      haskellPackages.cabal-fmt
      htop
      imagemagick
      jq
      librsvg # for the rsvg-convert binary
      libwebp
      luajit
      moreutils
      mosh
      ngrok
      optipng
      postgresql
      python3
      redis
      shfmt
      simple-http-server
      tree
      tree-sitter
      ttyd
      viu
      wget
      yq
      z-lua
      zlib
      zopfli
    ]
    # Haskell
    ++ (with pkgs.haskellPackages; [
      cabal-fmt
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
    ++ (with pkgs; [
      bash-language-server
      lua-language-server
      nil
      #semgrep # broken in nixpkgs unstable
      stylua
      typescript
      typescript-language-server
      vim-language-server
      vscode-langservers-extracted
      yaml-language-server
    ])
    # LSPs that use Node
    ++ (with pkgs.nodePackages_latest; [
      prettier
      svgo
    ])
    # Desktops only
    ++ (
      lib.optionals (! headless) (with pkgs; [
        vscode
      ])
    )
    # Linux Desktop
    ++ (lib.optionals (isLinux && ! headless) (with pkgs; [
      element
    ]))
    # macOS
    ++ (lib.optionals isDarwin (with pkgs; [
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
