{pkgs, ...}: {
  environment.variables.NIXOS_OZONE_WL = "1";

  # By default Discord requires that the latest version be installed. Turn that
  # off.
  environment.etc."discord/settings.json".text = ''{"SKIP_HOST_UPDATE": true}'';

  # Desktop entry with HiDPI scaling for native Wayland
  environment.systemPackages = with pkgs; [
    discord
    (makeDesktopItem {
      name = "discord";
      desktopName = "Discord";
      comment = "All-in-one voice and text chat for gamers";
      genericName = "Internet Messenger";
      exec = "discord --force-device-scale-factor=1.5 %U";
      icon = "discord";
      terminal = false;
      categories = ["Network" "InstantMessaging"];
      mimeTypes = ["x-scheme-handler/discord"];
    })
  ];
}
