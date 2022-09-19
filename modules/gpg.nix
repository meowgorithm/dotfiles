{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.gpg = {
    enable = true;
  };
}
