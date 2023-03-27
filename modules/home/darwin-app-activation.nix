# There's a lot to unpack about where this black magick came from, but here's
# the summary:
#
# https://github.com/caarlos0/dotfiles.nix/blob/d4ae1288df5da9057941907da0e2571c790b480c/modules/darwin-app-activation.nix
{
  config,
  lib,
  pkgs,
  inputs,
  system,
  ...
}: let
  mkalias = inputs.mkAlias.outputs.apps.${system}.default.program;
in {
  disabledModules = ["targets/darwin/linkapps.nix"];
  home.activation.aliasApplications =
    lib.mkIf pkgs.stdenv.hostPlatform.isDarwin
    (let
      apps = pkgs.buildEnv {
        name = "home-manager-applications";
        paths = config.home.packages;
        pathsToLink = "/Applications";
      };
    in
      lib.hm.dag.entryAfter ["linkGeneration"]
      "	echo \"Linking Home Manager applications...\" 2>&1\n\n	app_path=\"$HOME/Applications/Home Manager Apps\"\n	tmp_path=\"$(mktemp -dt \"home-manager-applications.XXXXXXXXXX\")\" || exit 1\n\n	${pkgs.fd}/bin/fd \\\n		-t l -d 1 . ${apps}/Applications \\\n		-x $DRY_RUN_CMD ${mkalias} -L {} \"$tmp_path/{/}\"\n\n	$DRY_RUN_CMD rm -rf \"$app_path\"\n	$DRY_RUN_CMD mv \"$tmp_path\" \"$app_path\"\n");
}
