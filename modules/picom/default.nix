{
  config,
  pkgs,
  lib,
  ...
}: {
  xdg.configFile."picom.conf".source = ./picom.conf;
}
