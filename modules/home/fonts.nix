{
  config,
  pkgs,
  lib,
  ...
}: {
  home.packages = with pkgs; [
    jetbrains-mono

    # External
    anchor
    arno-pro
    benjamins-gothic
    gabriello
    larsseit
    monoflow
    pique
    rifton
    rois
    sf-mono
    space-grotesk
    symbolset
    upton
    untitled-sans
  ];
}
