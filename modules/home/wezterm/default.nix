{...}: let
  colors = import ../colors.nix;
in {
  programs.wezterm = {
    enable = true;
    enableBashIntegration = true;
    colorSchemes = {
      "Pantera Negra" = {
        foreground = colors.primary.foreground;
        background = colors.primary.background;
        ansi = [
          colors.normal.black
          colors.normal.red
          colors.normal.green
          colors.normal.yellow
          colors.normal.blue
          colors.normal.magenta
          colors.normal.cyan
          colors.normal.white
        ];
        brights = [
          colors.bright.black
          colors.bright.red
          colors.bright.green
          colors.bright.yellow
          colors.bright.blue
          colors.bright.magenta
          colors.bright.cyan
          colors.bright.white
        ];
      };
    };
    extraConfig = builtins.readFile ./wezterm.lua;
  };
}
