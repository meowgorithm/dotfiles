{
  pkgs,
  inputs,
  ...
}: let
  fonts = [
    "anchor"
    "arno-pro"
    "benjamins-gothic"
    "gabriello"
    "gelion"
    "larsseit"
    "monoflow"
    "neufile-grotesk"
    "pique"
    "rifton"
    "rois"
    "sf-mono"
    "symbolset"
    "untitled-sans"
    "upton"
  ];

  mkFont = name:
    pkgs.stdenv.mkDerivation {
      inherit name;
      src = inputs."${name}";
      installPhase = ''
        mkdir -p $out/share/fonts/otf
        cp $src/* $out/share/fonts/otf
      '';
    };
in {
  home.packages = with pkgs;
    [
      inter
      jetbrains-mono
      ibm-plex
    ]
    ++ (map mkFont fonts);
}
