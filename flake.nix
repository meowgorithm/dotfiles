{
  description = "The Panther Room";

  inputs = {
    nixpkgs.url = "flake:nixpkgs";
    homeManager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, homeManager } @ inputs: {
    homeConfigurations =
      let
        system = "aarch64-darwin";
        pkgs = inputs.nixpkgs.legacyPackages."${system}";
      in
      {

        aarch64Darwin = homeManager.lib.homeManagerConfiguration {
          pkgs = pkgs;
          modules = [

            {
              home = {
                username = "christian";
                homeDirectory = "/Users/christian";
                stateVersion = "22.11";
              };
              programs = {
                home-manager.enable = true;
                bash = (import ./bash.nix) pkgs;
              };
            }

            ./pkgs.nix
            ./git.nix

          ];
        };
      };

    packages.aarch64-darwin.default = self.homeConfigurations.aarch64Darwin.activationPackage;
  };

}
