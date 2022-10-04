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
