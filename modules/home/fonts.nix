{
  pkgs,
  fonts,
  headless,
  ...
}: let
  maybeUse = pkgs.lib.optionals (! headless);
  allFonts = map (x: pkgs.${x}) fonts;
in {
  home.packages = maybeUse allFonts;
  fonts.fontconfig.enable = true;
}
