{pkgs, ...}: {
  xdg.configFile."hypr/hyprland.conf".source = ./hyprland.conf;

  home.packages = with pkgs; [
    waybar
  ];
}
