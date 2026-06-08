{pkgs, ...}: {
  environment.systemPackages = [
    (pkgs.zoom-us.override {
      hyprlandXdgDesktopPortalSupport = true;
    })
  ];

  # Ensure Zoom uses native Wayland for proper HiDPI rendering.
  systemd.user.services.zoom-wayland-fix = {
    description = "Set xwayland=false in zoomus.conf";
    wantedBy = ["graphical-session.target"];
    script = ''
      conf="$HOME/.config/zoomus.conf"
      if [ -f "$conf" ]; then
        ${pkgs.gnused}/bin/sed -i 's/^xwayland=true/xwayland=false/' "$conf"
      else
        mkdir -p "$HOME/.config"
        printf '[General]\nxwayland=false\n' > "$conf"
      fi
    '';
    serviceConfig.Type = "oneshot";
  };
}
