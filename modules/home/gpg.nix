{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    defaultCacheTtl = 60 * 60 * 8;
    maxCacheTtl = 60 * 60 * 8;
  };
}
