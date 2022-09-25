{ config
, pkgs
, lib
, ...
}: {
  home.file.".Xmodmap".source = ./Xmodmap;
  home.file.".Xresources".source = ./Xresources;
  xdg.configFile.".sx/sxrc".source = ./sxrc;
}
