{pkgs, ...}: {
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    dockerSocket.enable = true;
    defaultNetwork.settings.dns_enabled = true;
  };

  # Disable Docker compatibility notices.
  environment.variables.CONTAINERS_CONF_SUPPRESS_COMPAT_WARNINGS = "1";

  environment.systemPackages = with pkgs; [
    podman-compose
    podman-tui
  ];

  # Provides podman.socket. In your shell env you'll need to:
  # export DOCKER_HOST=unix://$XDG_RUNTIME_DIR/podman/podman.sock
  users.users.christian.extraGroups = ["podman"];
  systemd.user.services.podman.enable = true;
}
