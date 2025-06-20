{pkgs, ...}: {
  programs.firefox = {
    enable = true;
    profiles.default = {
      settings = {
        # Scale on Linux for HiDPI displays
        "layout.css.devPixelsPerPx" = "2.5";
      };
    };
  };
}

