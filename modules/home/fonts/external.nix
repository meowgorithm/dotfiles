self: super: let
  font = name: rev:
    self.stdenv.mkDerivation {
      inherit name;
      pname = name;
      src = builtins.fetchGit {
        inherit rev;
        url = "ssh://git.rocha.is/${name}";
      };
      installPhase = ''
        mkdir -p $out/share/fonts/otf
        cp $src/* $out/share/fonts/otf
      '';
    };
in {
  anchor = font "anchor" "930d232fcdaa570622e57e6d619f8cafc87a42a5";
  arno-pro = font "arno-pro" "76e7c3acd621b36fcf62490ff33071b90a7c138f";
  benjamins-gothic = font "benjamins-gothic" "6b5af96f1a58061d0621830aaa828b8b98a75865";
  gabriello = font "gabriello" "72e5c2bc8eeada449833ae86a69892d20ddfe3da";
  larsseit = font "larsseit" "26f75356e8aa42c335eb3ea63c9ea08b33552993";
  monoflow = font "monoflow" "9d6135427c301d49141f4035b2dd4f1c01ef09e2";
  sf-mono = font "sf-mono" "a339697b71c771aff4aa4e130b41628002781536";
  symbolset = font "symbolset" "8ea7db33949d3f6258b3d4379f57c821b74273ad";
  rifton = font "rifton" "8a1f1dcd6b065037a9496659bd93f16f46c734e4";
  rois = font "rois" "ec4dff9e1f172f152b4114a7268e450203bbd631";
  upton = font "upton" "f619cc4e209f2fc691f9429d923463602c00470a";
  untitled-sans = font "untitled-sans" "581a33677a16c100c4077d34b3e9d5b91d2f6ef1";
}
