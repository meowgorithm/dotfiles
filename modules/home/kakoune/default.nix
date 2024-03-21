{pkgs, ...}: {
  programs.kakoune = {
    enable = true;
    extraConfig = builtins.readFile ./kakrc;
  };

  home.packages = with pkgs; [
    kak-lsp
  ];
}
