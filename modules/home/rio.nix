{pkgs, ...}: {
  home.packages = [pkgs.rio];

  xdg.configFile."rio/config.toml".text = let
    colors =
      import ./colors.nix;

    home =
      if pkgs.stdenv.isDarwin
      then "/Users/christian"
      else "/home/christian";

    fontSize =
      if pkgs.stdenv.isDarwin
      then "13"
      else "7";
  in ''
    hide-cursor-when-typing = true
    padding-x = 16
    padding-y = [0, 8]
    option-as-alt = 'left'
    confirm-before-quit = false

    [editor]
    program = "${home}/.nix-profile/bin/nvim"
    args = []

    [window]
    width = 600
    height = 640
    mode = "Windowed"
    opacity = 1.0
    blur = false
    decorations = "Buttonless"

    [cursor]
    blinking = true
    blinking-interval = 530

    line-height = 1.2

    [fonts]
    size = ${fontSize}

    [fonts.regular]
    family = "JetBrains Mono"
    style = "Normal"
    weight = 400

    [fonts.bold]
    family = "JetBrains Mono"
    style = "Normal"
    weight = 800

    [fonts.italic]
    family = "JetBrains Mono"
    style = "Italic"
    weight = 400

    [fonts.bold-italic]
    family = "JetBrains Mono"
    style = "Italic"
    weight = 800

    [navigation]
    mode = "Plain"

    [colors]
    cursor = '#00af87'
    background = '${colors.background}'
    foreground = '${colors.foreground}'
    # cursor = '#F38BA3'
    # tabs = '#443d40'
    # tabs-active = '#F38BA3'
    # green = '#0BA95B'
    # red = '#ED203D'
    # blue = '#12B5E5'
    # yellow = '#FCBA28'
  '';
}
