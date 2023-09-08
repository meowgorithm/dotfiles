{
  pkgs,
  inputs,
  ...
}: {
  home.packages = with pkgs; [hammerspoon];
  home.file.".hammerspoon/init.lua".source = ./init.lua;
  home.file.".hammerspoon/rc.lua.gpg".source = ./rc.lua.gpg;
  home.file.".hammerspoon/fzy.lua".source = inputs.fzyLua;
}
