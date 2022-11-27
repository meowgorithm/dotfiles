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
}
