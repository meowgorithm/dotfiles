{
  config,
  pkgs,
  lib,
  ...
}: {
  services.picom = {
    enable = true;
    shadow = true;
    shadowOffsets = [(-60) (-45)];
    shadowOpacity = 0.8;
    fade = true;
    fadeSteps = [0.24 0.24];
    fadeDelta = 16;
    settings = {
      shadow-radius = 60;
      no-dock-shadow = true;
      no-dnd-shadow = true;
    };
  };
}
