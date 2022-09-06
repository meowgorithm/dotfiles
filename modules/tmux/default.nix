{
  config,
  pkgs,
  lib,
  ...
}: {
  # Home Manager's tmux flake inserts some settings we don't want, so just use
  # a simple config file in this case.
  home.file.".tmux.conf".source = ./tmux.conf;
}
