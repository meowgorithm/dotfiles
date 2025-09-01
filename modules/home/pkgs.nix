{
  pkgs,
  lib,
  charmPkgs,
  carlosPkgs,
  ...
}: let
  isDarwin = pkgs.stdenv.isDarwin;
in {
  home.packages = with pkgs;
  # macOS
    (lib.optionals isDarwin (with pkgs; [
      cachix
      dozer
      monitorcontrol
    ]))
    # Charm NUR
    ++ (map (x: pkgs.${x}) charmPkgs)
    # Carlos' NUR
    ++ (map (x: pkgs.${x}) carlosPkgs);

  programs = {
    z-lua.enable = true;
  };
}
