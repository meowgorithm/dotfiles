{
  pkgs,
  inputs,
  ...
}: {
  home.packages = with pkgs; [hammerspoon];
  home.file.".hammerspoon/init.lua".source = ./init.lua;
}
