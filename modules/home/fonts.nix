{
  config,
  pkgs,
  lib,
  ...
}: {
  home.packages = with pkgs; [
    inter
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
    symbolset
    upton
    untitled-sans
  ];
}
