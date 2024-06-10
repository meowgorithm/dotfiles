{pkgs, ...}: let
  colors = import ./colors.nix;
in {
  xdg.configFile."ghostty/config".text =
    ''
      term = xterm-256color

      macos-non-native-fullscreen = true
      macos-option-as-alt = left

      mouse-hide-while-typing = true
      copy-on-select = true
      confirm-close-surface = false
      link-url = true

      background = ${colors.primary.background}
      foreground = ${colors.primary.foreground}

      palette = 0=${colors.normal.black}
      palette = 1=${colors.normal.red}
      palette = 2=${colors.normal.green}
      palette = 3=${colors.normal.yellow}
      palette = 4=${colors.normal.blue}
      palette = 5=${colors.normal.magenta}
      palette = 6=${colors.normal.cyan}
      palette = 7=${colors.normal.white}
      palette = 8=${colors.bright.black}
      palette = 9=${colors.bright.red}
      palette = 10=${colors.bright.green}
      palette = 11=${colors.bright.yellow}
      palette = 12=${colors.bright.blue}
      palette = 13=${colors.bright.magenta}
      palette = 14=${colors.bright.cyan}
      palette = 15=${colors.bright.white}
    ''
    + (
      if pkgs.stdenv.isDarwin
      then ''
        font-family = JetBrains Mono Light
        font-family-bold = JetBrains Mono Bold
        font-family-italic = JetBrains Mono Italic
        font-family-bold-italic = JetBrains Mono Bold Italic
        font-size = 13
        font-thicken = true
        adjust-cursor-thickness = 175%
        adjust-cell-width = -4%
        window-padding-x = 12
        window-padding-y = 10
        keybind = super+f=toggle_fullscreen
        window-colorspace = display-p3
      ''
      else ''
        window-decoration = false
        font-family = JetBrains Mono
        font-size = 5
        window-padding-x = 6
        window-padding-y = 5
      ''
    );
}
