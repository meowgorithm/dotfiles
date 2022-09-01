{ config, pkgs, lib, ... }:
let

  nix = with pkgs; [
    rnix-lsp
    alejandra
  ];

  base = with pkgs; [
    aws
    brotli
    ctags
    direnv
    exa
    ffmpeg
    gh
    gnupg
    gopass
    jq
    kak-lsp
    kakoune
    libwebp
    neovim
    nodejs-16_x
    ripgrep
    shellcheck
    tmux
    tree
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
    skate
  ];

in
{
  home.packages =
    nix ++ base ++ go ++ charm;

  programs =
    {
      z-lua.enable = true;
    };
}
