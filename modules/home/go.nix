{pkgs, ...}: {
  programs.go = {
    enable = true;
    package = pkgs.go_1_24;
    goPath = ".go";
    goPrivate = [
      "github.com/charmbracelet"
      "github.com/meowgorithm"
    ];
  };

  home.packages = with pkgs; [
    gofumpt
    delve
    golangci-lint
    gopls
    goreleaser
    gotools
  ];
}
