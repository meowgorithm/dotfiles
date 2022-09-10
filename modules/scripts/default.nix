{
  config,
  pkgs,
  lib,
  ...
}: {
  home.file = {
    ".bin/make-vid".source = ./make-vid;
    ".bin/install-docker".source = ./install-docker;
  };
}
