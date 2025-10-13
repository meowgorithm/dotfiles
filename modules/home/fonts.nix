{
  pkgs,
  fonts,
  ...
}: {
  home.packages = map (x: pkgs.${x}) fonts;
  fonts.fontconfig.enable = true;
}
