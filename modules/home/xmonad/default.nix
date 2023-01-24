{
  config,
  pkgs,
  lib,
  ...
}: {
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad.hs;
  };

  programs.xmobar = {
    enable = true;
    extraConfig = builtins.readFile ./xmobar.hs;
  };
}
