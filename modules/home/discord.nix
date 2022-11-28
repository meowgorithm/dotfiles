{ config
, pkgs
, lib
, ...
}: {
  # By default Discord requires that the latest version be installed. Turn that
  # off.
  xdg.configFile."discord/settings.json".text = ''{"SKIP_HOST_UPDATE": true}'';
}
