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
    haskellPackages.cabal-fmt
    cargo
    coreutils
    ctags
    curl
    direnv
    duf
    exa
    ffmpeg
    fzf
    gh
    gnupg
    gopass
    htop
    jq
    kak-lsp
    kakoune
    libwebp
    luajit
    moreutils
    neovim
    nodejs-16_x
    python3
    ripgrep
    shellcheck
    shfmt
    tmux
    tree
    ttyd
    vim
    viu
    wget
    yq
    z-lua
    zopfli
  ];

  go = with pkgs; [
    go_1_19
    golangci-lint
    goreleaser
  ];

  lsp = with pkgs;
  with pkgs.elmPackages;
  with pkgs.nodePackages; [
    alejandra
    bash-language-server
    elm-language-server
    prettier
    rnix-lsp
    svgo
    typescript
    typescript-language-server
    vscode-css-languageserver-bin
    vscode-html-languageserver-bin
    yaml-language-server
  ];
in {
  home.packages = base ++ go ++ lsp;

  programs = {
    z-lua.enable = true;
  };
}
