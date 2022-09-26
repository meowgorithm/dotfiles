{
  config,
  pkgs,
  lib,
  ...
}: let
  nix = with pkgs; [
    rnix-lsp
    alejandra
  ];

  base = with pkgs; [
    age
    brotli
    cargo
    coreutils
    ctags
    curl
    direnv
    duf
    exa
    ffmpeg
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
    ripgrep
    shellcheck
    shfmt
    tmux
    tree
    ttyd
    vim
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

  charm = with pkgs; [
    glow
    gum
    melt
    skate
  ];
in {
  home.packages = nix ++ base ++ go ++ charm;

  programs = {
    z-lua.enable = true;
  };
}
