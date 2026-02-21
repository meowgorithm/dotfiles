{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = {nixpkgs, ...}: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in {
    packages.x86_64-linux.default = pkgs.buildEnv {
      name = "meowgorithm";
      paths = with pkgs; [
        alejandra
        haskellPackages.cabal-fmt
        haskellPackages.fourmolu
      ];
    };
  };
}
