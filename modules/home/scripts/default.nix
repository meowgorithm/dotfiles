{
  config,
  pkgs,
  lib,
  ...
}: {
  home.file =
    {
      ".bin/guard".source = ./guard;
      ".bin/install-docker".source = ./install-docker;
      ".bin/make-vid".source = ./make-vid;
      ".bin/sessions".source = ./sessions;
      ".bin/sessions.gpg".source = ./sessions.gpg;
      ".bin/tm".source = ./tm;
      ".bin/tmls".source = ./tmls;
      ".bin/webp-jpeg".source = ./webp-jpeg;
      ".bin/webp-png".source = ./webp-png;
    }
    // (
      if pkgs.stdenv.isDarwin
      then {
        ".bin/make-aliases".source = ./make-aliases;
      }
      else {
        ".bin/map-keys".source = ./map-keys;
        ".bin/setup-mouse".source = ./setup-mouse;
        ".bin/setup-wacom".source = ./setup-wacom;
      }
    );
}
