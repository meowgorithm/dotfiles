{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.helix = {
    enable = true;

    settings = {
      keys.normal = {
        g.q = ":reflow";
        X = "extend_line_above";
        "minus" = "goto_prev_paragraph";
        "=" = "goto_next_paragraph";
      };
    };

    languages = [
      {
        name = "nix";
        auto-format = true;
        formatter = {command = "alejandra";};
      }
    ];
  };
}
