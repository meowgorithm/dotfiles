{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.readline = {
    enable = true;
    includeSystemConfig = false;
    variables.expand-tilde = true;
  };
}
