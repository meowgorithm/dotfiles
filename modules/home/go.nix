{pkgs, ...}: {
  programs.go = {
    enable = true;
    package = pkgs.go;
    goPath = "$HOME/.go";
    goPrivate = [
      "github.com/charmbracelet"
      "github.com/meowgorithm"
    ];
  };

  home.packages = with pkgs; [
    delve
    golangci-lint
    gopls
    goreleaser
    gotools
  ];
}
