{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.helix = {
    enable = true;

    settings = {
      theme = "noctis_bordo_adjusted";
      keys.normal = {
        g.q = ":reflow";
        X = "extend_line_above";
        "minus" = "goto_prev_paragraph";
        "=" = "goto_next_paragraph";
      };
      editor = {
        cursorline = true;
      };
    };

    languages = [
      {
        name = "nix";
        auto-format = true;
        formatter = {command = "alejandra";};
      }
    ];

    themes = {
      noctis_bordo_adjusted = import ./noctis_bordo_adjusted.nix;
    };
  };
}
