{...}: {
  programs.zellij = {
    enable = true;
    enableBashIntegration = false; # don't auto-start zellij
  };
}
