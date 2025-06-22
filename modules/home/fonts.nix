{
  pkgs,
  fonts,
  headless,
  ...
}: let
  maybeUse = pkgs.lib.optionals (! headless);

  allFonts = with pkgs;
    [
      inter
      jetbrains-mono
      ibm-plex
    ]
    ++ (map (x: pkgs.${x}) fonts);
in {
  home.packages = maybeUse allFonts;

  fonts.fontconfig.enable = true;
}
