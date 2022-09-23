{
  config,
  pkgs,
  lib,
  ...
}: {
  home.file =
    {
      ".bin/install-docker".source = ./install-docker;
      ".bin/make-vid".source = ./make-vid;
      ".bin/tm".source = ./tm;
      ".bin/tmls".source = ./tmls;
    }
    // (
      if pkgs.stdenv.isDarwin
      then {}
      else {
        ".bin/map-keys".source = ./map-keys;
        ".bin/setup-mouse".source = ./setup-mouse;
        ".bin/setup-wacom".source = ./setup-wacom;
      }
    );
}
