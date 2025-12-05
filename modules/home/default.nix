{
  home-manager,
  pkgs,
  system,
  inputs,
}:
home-manager.lib.homeManagerConfiguration {
  inherit pkgs;
  modules = [
    rec {
      home.stateVersion = "22.11";
      home.username = "christian";
      home.homeDirectory =
        (
          if pkgs.stdenv.isDarwin
          then "/Users/"
          else "/home/"
        )
        + home.username;
    }
  ];
}
