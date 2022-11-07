helixPackage: {
  config,
  pkgs,
  lib,
  helix,
  ...
}: {
  programs.helix = {
    enable = true;
    package = helixPackage;

    settings = {
      theme = "base16_default";
      keys.normal = {
        g.q = ":reflow";
        X = "extend_line_above";
        "minus" = "goto_prev_paragraph";
        "=" = "goto_next_paragraph";
      };
      editor = {
        cursorline = true;
        true-color = true;
        cursor-shape.insert = "bar";
        lsp.display-messages = true;
        rulers = [80];
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
