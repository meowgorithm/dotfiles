{pkgs, ...}: {
  home.file.".hammerspoon/init.lua".source = ./init.lua;
  home.packages = with pkgs; [hammerspoon];
}
