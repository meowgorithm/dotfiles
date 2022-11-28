{
  config,
  pkgs,
  lib,
  ...
}: {
  xdg.configFile.".floskell.json".source = ./floskell.json;
}
