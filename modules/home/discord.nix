{...}: {
  # By default Discord requires that the latest version be installed. Turn that
  # off.
  xdg.configFile."discord/settings.json".text = ''{"SKIP_HOST_UPDATE": true}'';

  # Desktop entry with proper scaling
  xdg.desktopEntries."discord" = {
    name = "Discord";
    comment = "All-in-one voice and text chat for gamers";
    genericName = "Internet Messenger";
    exec = "discord --force-device-scale-factor=1.5 --high-dpi-support=1 --force-color-profile=srgb %U";
    icon = "discord";
    terminal = false;
    categories = ["Network" "InstantMessaging"];
    mimeType = ["x-scheme-handler/discord"];
  };
}
