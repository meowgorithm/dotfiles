{
  config,
  pkgs,
  lib,
  ...
}: let
  base = with pkgs; [
    age
    awscli2
    bashInteractive
    brotli
    cargo
    coreutils
    ctags
    curl
    direnv
    duf
    exa
    ffmpeg
    fish
    fzf
    gh
    gnupg
    gnutar
    gopass
    htop
    imagemagick
    jq
    kak-lsp
    kakoune
    libwebp
    luajit
    moreutils
    nodejs-16_x
    optipng
    python3
    ripgrep
    shellcheck
    shfmt
    simple-http-server
    tmux
    tree
    ttyd
    vim
    viu
    wget
    yq
    yubikey-manager
    z-lua
    zopfli
  ];

  go = with pkgs; [
    delve
    go_1_19
    golangci-lint
    gopls
    goreleaser
    gotools
  ];

  haskell = with pkgs.haskellPackages;
    [
      cabal-fmt
      cabal-install
      floskell
      fourmolu
      pkgs.haskell.compiler.ghc943
    ]
    ++ (
      if pkgs.stdenv.isDarwin && pkgs.stdenv.isAarch64
      then []
      else [
        haskell-language-server
      ]
    );

  lsp = with pkgs;
  with pkgs.elmPackages;
  with pkgs.nodePackages_latest; [
    alejandra
    bash-language-server
    elm
    elm-language-server
    nil
    prettier
    svgo
    typescript
    typescript-language-server
    vscode-langservers-extracted
    yaml-language-server
  ];

  macos = with pkgs; [
    cachix
  ];
in {
  home.packages = base ++ go ++ haskell ++ lsp ++ macos;

  programs = {
    z-lua.enable = true;
  };
}
