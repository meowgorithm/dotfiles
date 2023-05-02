{
  pkgs,
  fonts,
  ...
}: {
  home.packages = with pkgs;
    [
      inter
      jetbrains-mono
      ibm-plex
    ]
    ++ (map (x: pkgs.${x}) fonts);
}
