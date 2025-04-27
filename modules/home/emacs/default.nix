{pkgs, ...}: {
  programs.emacs = {
    enable = false;
    package = pkgs.emacs;
    extraPackages = emacsPkgs:
      with emacsPkgs; [
        go-mode
        lsp-mode
        lsp-ui
        company
        yasnippet
      ];
    extraConfig = builtins.readFile ./init.el;
  };

  home.file.".emacs".text = ''
    (setq inhibit-startup-message t)
  '';
}
