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
    clang
    direnv
    doctl
    duf
    element
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
    libwebp
    lsd
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

  rust = with pkgs; [
    rust-analyzer
    rustc
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
      # fourmolu_0_13_1_0
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
    semgrep
    stylua
    svgo
    typescript
    typescript-language-server
    vim-language-server
    vscode-langservers-extracted
    yaml-language-server
  ];

  x11Dev = lib.optionals pkgs.stdenv.isLinux (with pkgs; [
    libGL
    xorg.libXcursor
    xorg.libXi
    xorg.libXinerama
    xorg.libXrandr
    xorg.libXxf86vm
  ]);

  charmNur = with pkgs; [
    gum
    melt
    mods
    pop
    soft-serve
    vhs
    wishlist
  ];

  maybeMacOS = lib.optionals pkgs.stdenv.isDarwin (with pkgs; [
    blender
    cachix
    dozer
    monitorcontrol
    nightfall
  ]);
in {
  home.packages = base ++ haskell ++ rust ++ lsp ++ maybeMacOS ++ x11Dev ++ charmNur;

  programs = {
    z-lua.enable = true;
  };
}
