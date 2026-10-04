{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = {nixpkgs, ...}: let
    supportedSystems = [
      "x86_64-linux"
      "aarch64-darwin"
    ];

    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
  in {
    packages = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      default = pkgs.buildEnv {
        name = "meowgorithm";
        paths = with pkgs;
          [
            alejandra
            nil
            sqlc
            tree-sitter
          ]
          ++ (with haskellPackages; [
            cabal-fmt
            fourmolu
          ])
          ++ (with pkgs.elmPackages; [
            elm-language-server
            elm-format
            elm-review
            elm-test
            elm
          ]);
      };
    });
  };
}
