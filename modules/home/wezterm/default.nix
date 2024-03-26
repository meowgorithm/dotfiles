{pkgs, ...}: let
  colors = import ../colors.nix;
in {
  programs.wezterm = {
    enable = !pkgs.stdenv.isDarwin;
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
    extraConfig =
      ({
        fontSize,
        cellWidth,
      }: ''
        return (function()
        	local wezterm = require("wezterm")
        	local config = {}

        	if wezterm.config_builder then
        		config = wezterm.config_builder()
        	end

        	config.font = wezterm.font({
        		family = "JetBrains Mono",
        		weight = "Medium",
        	})
        	config.font_size = ${fontSize}
        	config.line_height = 1.0
        	config.cell_width = ${cellWidth}
        	config.hide_tab_bar_if_only_one_tab = true
        	config.window_decorations = "RESIZE"
        	config.color_scheme = "Pantera Negra"
        	config.window_padding = {
        		left = "16pt",
        		right = "16pt",
        		top = "12pt",
        		bottom = "12pt",
        	}

        	return config
        end)()
      '')
      (
        if pkgs.stdenv.isLinux
        then {
          fontSize = "9.5";
          cellWidth = "1.0";
        }
        else {
          fontSize = "13.0";
          cellWidth = "0.9";
        }
      );
  };
}
