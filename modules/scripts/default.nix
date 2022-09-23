{
  config,
  pkgs,
  lib,
  ...
}: {
  home.file = {
    ".bin/install-docker".source = ./install-docker;
    ".bin/make-vid".source = ./make-vid;
    ".bin/tm".source = ./tm;
    ".bin/tmls".source = ./tmls;
  };
}
