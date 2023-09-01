{pkgs, ...}: let
  isDarwin =
    pkgs.stdenv.isDarwin;

  colors =
    import ./colors.nix;
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
          window_padding_width = "10.0 12.0";

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

          macos_option_as_alt = "left";
          macos_quit_when_last_window_closed = "yes";
          macos_traditional_fullscreen = "yes";
          macos_titlebar_color = "background";
          macos_show_window_title_in = "none";

          selection_foreground = "#222222";
          selection_background = "#ccff00";

          background = colors.primary.background;
          foreground = colors.primary.foreground;

          color0 = colors.normal.black;
          color8 = colors.bright.black;
          color1 = colors.normal.red;
          color9 = colors.bright.red;
          color2 = colors.normal.green;
          color10 = colors.bright.green;
          color3 = colors.normal.yellow;
          color11 = colors.bright.yellow;
          color4 = colors.normal.blue;
          color12 = colors.bright.blue;
          color5 = colors.normal.magenta;
          color13 = colors.bright.magenta;
          color6 = colors.normal.cyan;
          color14 = colors.bright.cyan;
          color7 = colors.normal.white;
          color15 = colors.bright.white;

          adjust_line_height = "105%";
        }
        // (
          if isDarwin
          then {
            font_family = "JetBrains Mono Light";
            italic_family = "JetBrains Mono Light Italic";
            bold_font = "JetBrains Mono Bold";
            bold_italic_font = "JetBrains Mono Medium Bold";

            font_size = "12.5";
            mouse_hide_wait = "3.0";
            kitty_mod = "cmd";
          }
          else {
            font_family = "JetBrains Mono";
            italic_family = "JetBrains Mono Italic";
            bold_font = "JetBrains Mono Bold";
            bold_italic_font = "JetBrains Mono Bold Italic";

            font_size = "9.0";
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
