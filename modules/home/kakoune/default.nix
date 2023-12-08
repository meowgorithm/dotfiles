{pkgs, ...}: let
  enable = !pkgs.stdenv.isDarwin;
in {
  programs.kakoune = {
    enable = enable;
    extraConfig = builtins.readFile ./kakrc;
  };

  home.packages = with pkgs;
    if enable
    then [
      kak-lsp
    ]
    else [];
}
