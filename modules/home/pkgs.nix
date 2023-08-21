{
  pkgs,
  lib,
  ...
}: let
  base = with pkgs; [
    age
    awscli2
    bashInteractive
    bc
    brotli
    cargo
    coreutils
    ctags
    curl
    direnv
    duf
    element
    exa
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
    kak-lsp
    kakoune
    libwebp
    luajit
    moreutils
    ngrok
    nodejs
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
  ];

  charmNur = with pkgs; [
    gum
    pop
    soft-serve
    vhs
    wishlist
  ];

  go = with pkgs; [
    delve
    go_1_20
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
      pkgs.haskell.compiler.ghc943
    ]
    ++ lib.optionals (pkgs.stdenv.isDarwin != true)
    [
      fourmolu_0_13_1_0
    ]
    ++ lib.optionals ((pkgs.stdenv.isDarwin && pkgs.stdenv.isAarch64) != true)
    [
      haskell-language-server
    ];

  lsp = with pkgs;
  with pkgs.elmPackages;
  with pkgs.nodePackages_latest; [
    alejandra
    bash-language-server
    elm
    elm-language-server
    golangci-lint-langserver
    lua-language-server
    nil
    prettier
    stylua
    svgo
    typescript
    typescript-language-server
    vim-language-server
    vscode-langservers-extracted
    yaml-language-server
  ];

  maybeMacOS = lib.optionals pkgs.stdenv.isDarwin (with pkgs; [
    blender
    cachix
    dozer
    nightfall
  ]);
in {
  home.packages = base ++ go ++ haskell ++ lsp ++ maybeMacOS ++ charmNur;

  programs = {
    z-lua.enable = true;
  };
}
