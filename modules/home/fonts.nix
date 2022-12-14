{
  config,
  pkgs,
  lib,
  ...
}: {
  home.packages = with pkgs; [
    jetbrains-mono
  ];
}
