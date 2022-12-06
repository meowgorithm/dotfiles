{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.rofi = {
    enable = true;
    extraConfig = {
      padding = 40;
      width = 1200;
      lines = 20;
      dpi = 192;
      font = "Inter 12";
    };
    theme = ./theme.rasi;
  };
}
