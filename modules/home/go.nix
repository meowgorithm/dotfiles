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
    delve
    gofumpt
    golangci-lint
    golangci-lint-langserver
    gopls
    goreleaser
    gotools
  ];
}
