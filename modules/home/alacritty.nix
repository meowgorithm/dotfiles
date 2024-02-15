{pkgs, ...} @ inputs: let
  isDarwin =
    inputs.pkgs.stdenv.isDarwin;
in {
  programs.alacritty = {
    enable = true;

    settings = {
      window =
        {
          padding = {
            x = 10;
            y = 16;
          };
        }
        // (
          if isDarwin
          then {decorations = "buttonless";}
          else {}
        );

      font = let
        fontFamily = "JetBrains Mono";
      in
        {
          normal = {
            family = fontFamily;
            style = "Light";
          };
          bold = {
            family = fontFamily;
            style = "Bold";
          };
          italic = {
            family = fontFamily;
            style = "Bold Italic";
          };
          bold_italic = {
            family = fontFamily;
            style = "Bold Italic";
          };
        }
        // (
          if isDarwin
          then {
            size = 12.0;
            offset = {
              x = 0;
              y = 2;
            };
            glyph_offset = {
              x = 0;
              y = 3;
            };
          }
          else {
            size = 10.0;
          }
        );

      colors = import ./colors.nix;
    };
  };
}
