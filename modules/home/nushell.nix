{pkgs, ...}: {
  home.packages = with pkgs;
  with pkgs.nushellPlugins; [
    nufmt
    gstat
    highlight
  ];

  programs.nushell = {
    enable = true;
    settings = {
      show_banner = false;
    };
    extraConfig = ''
    '';
  };
}
