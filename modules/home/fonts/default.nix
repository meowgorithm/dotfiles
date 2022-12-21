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
    rifton
    rois
    sf-mono
    symbolset
    upton
    untitled-sans
  ];
}
