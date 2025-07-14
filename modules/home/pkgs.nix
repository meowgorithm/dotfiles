{
  pkgs,
  charmPkgs,
  carlosPkgs,
  ...
}: {
  home.packages = with pkgs;
    [
      cargo
      clang
      coreutils
      ctags
      getopt
      gnumake
      gnupg
      gnused
      gnutar
      htop
      imagemagick
      librsvg # for the rsvg-convert binary
      libwebp
      luajit
      moreutils
      python3
      tree-sitter
    ]
    # Haskell
    ++ (with pkgs.haskellPackages; [
      cabal-fmt
    ])
    # Elm
    ++ (with pkgs.elmPackages; [
      elm
      elm-language-server
    ])
    # Various LSP
    ++ (with pkgs; [
      nil
    ])
    # Charm NUR
    ++ (map (x: pkgs.${x}) charmPkgs)
    # Carlos' NUR
    ++ (map (x: pkgs.${x}) carlosPkgs);

  programs = {
    z-lua.enable = true;
  };
}
