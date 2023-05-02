{pkgs, ...}: {
  home.packages = with pkgs; [
    inter
    jetbrains-mono
    ibm-plex

    # External
    anchor
    arno-pro
    benjamins-gothic
    gabriello
    gelion
    larsseit
    monoflow
    neufile-grotesk
    pique
    rifton
    rois
    sf-mono
    symbolset
    upton
    untitled-sans
  ];
}
