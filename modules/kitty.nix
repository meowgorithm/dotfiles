{
  config,
  pkgs,
  lib,
  ...
}: let
  isDarwin = pkgs.stdenv.isDarwin;
in {
  programs.kitty =
    (
      if isDarwin
      then {
        darwinLaunchOptions = [];
      }
      else {}
    )
    // {
      enable = true;

      environment = {
        PATH = "/usr/local/bin:\${PATH}";
      };

      settings =
        {
          enable_audio_bell = false;
          copy_on_select = true;
          remember_window_size = false;
          initial_window_width = 680;
          initial_window_height = 720;

          enabled_layouts = "tall:bias=70, tall:bias=30, horizontal, stack";

          window_border_width = "1.0";
          window_margin_width = "0.0";
          single_window_margin_width = "0.0";
          window_padding_width = "8.0";

          active_border_color = "#686868";
          inactive_border_color = "#4A4A4A";

          tab_bar_margin_width = "1.0";
          tab_bar_style = "separator";
          tab_separator = "\" │ \"";

          active_tab_foreground = "#e6e6e6";
          active_tab_background = "#3a3a3a";
          active_tab_font_style = "normal";
          inactive_tab_foreground = "#bfbfbf";
          inactive_tab_background = "#171717";
          inactive_tab_font_style = "normal";

          allow_remote_control = true;

          macos_titlebar_color = "background";
          macos_option_as_alt = "left";
          macos_quit_when_last_window_closed = "yes";
          macos_traditional_fullscreen = "yes";
          macos_show_window_title_in = "none";

          # Colors {{{

          selection_foreground = "#222222";
          selection_background = "#ccff00";

          foreground = "#dddddd";
          background = "#171717";

          # black
          color0 = "#000000";
          color8 = "#4d4d4d";

          # red
          color1 = "#c73b1d";
          color9 = "#e82100";

          # green
          color2 = "#00a800";
          color10 = "#00db00";

          # yellow
          color3 = "#acaf15";
          color11 = "#e5e900";

          # blue
          color4 = "#3854FC";
          color12 = "#566BF9";

          # magenta
          color5 = "#d533ce";
          color13 = "#e83ae9";

          # cyan
          color6 = "#2cbac9";
          color14 = "#00e6e7";

          # white
          color7 = "#bfbfbf";
          color15 = "#e6e6e6";
        }
        // (
          if isDarwin
          then {
            font_family = "SF Mono Medium";
            italic_font = "SF Mono Medium Italic";
            bold_font = "SF Mono Heavy";
            bold_italic_font = "SF Mono Heavy Italic";

            font_size = "12.0";
            adjust_line_height = "120%";

            mouse_hide_wait = "3.0";

            kitty_mod = "cmd";
          }
          else {
            font_family = "JetBrains Mono";
            italic_family = "JetBrains Mono Italic";
            bold_font = "JetBrains Mono Bold";
            bold_italic_font = "JetBrains Mono Bold Italic";

            font_size = "9.0";
            adjust_line_height = "105%";

            background_opacity = "0.97";
            dynamic_background_opacity = "yes";
          }
        );

      keybindings =
        if isDarwin
        then {
          "kitty_mod+n" = "new_os_window_with_cwd";
          "kitty_mod+t" = "new_tab_with_cwd";
          "kitty_mod+shift+." = "move_tab_forward";
          "kitty_mod+shift+," = "move_tab_backward";

          # Create windows
          "cmd+enter" = "launch --cwd=current --location=vsplit";
          "shift+cmd+enter" = "launch --cwd=current --location=hsplit";

          # Select window (BSP-layout only)
          "cmd+shift+h" = "neighboring_window left";
          "cmd+shift+l" = "neighboring_window right";
          "cmd+shift+k" = "neighboring_window up";
          "cmd+shift+j" = "neighboring_window down";

          # Move windows
          "cmd+b" = "layout_action rotate";
          "cmd+ctrl+k" = "move_window up";
          "cmd+ctrl+h" = "move_window left";
          "cmd+ctrl+l" = "move_window right";
          "cmd+ctrl+j" = "move_window down";

          # Resize windows
          "cmd+shift+ctrl+h" = "resize_window narrower 4";
          "cmd+shift+ctrl+l" = "resize_window wider 4";
          "cmd+shift+ctrl+k" = "resize_window taller 4";
          "cmd+shift+ctrl+j" = "resize_window shorter 4";
        }
        else {};
    };
}
