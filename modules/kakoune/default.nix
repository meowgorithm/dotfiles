{ config
, pkgs
, lib
, ...
}: {
  programs.kakoune = {
    enable = true;
    extraConfig = builtins.readFile ./kakrc;
  };
}
