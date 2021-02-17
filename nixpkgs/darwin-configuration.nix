{ config, pkgs, ... }:

{
  imports = [ <home-manager/nix-darwin> ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [ pkgs.ripgrep
      pkgs.wget
      pkgs.tree
      pkgs.fzf
      pkgs.brotli
      pkgs.zopfli
      pkgs.duf
      pkgs.htop
      pkgs.direnv
      pkgs.ctags
      pkgs.upx
      pkgs.less
      pkgs.tmux
      pkgs.jq
      pkgs.rnix-lsp # nix language server
      pkgs.gopass
      pkgs.shellcheck
    ];

  home-manager.useUserPackages = false;
  home-manager.useGlobalPkgs = true;

  users.users.christian.name = "christian";
  users.users.christian.home = "/Users/christian";
  home-manager.users.christian = { pkgs, ... }: {

    programs.git = {

      enable = true;
      package = pkgs.gitFull;
      userName = "Christian Rocha";
      userEmail = "christian@rocha.is";

      signing = {
        key = "782ABD7E56DD173F";
        signByDefault = true;
      };

      aliases = {
        st = "status";
        ci = "commit";
        co = "checkout";
        i = "instaweb";
        count = "!git log --pretty=oneline | wc -l";
        housekeeping = "!git fsck && git fsck --unreachable && git gc --aggressive --prune && git prune && git prune-packed";
        lg1 = "log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all";
        lg2 = "log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n''		   %C(white)%s%C(reset) %C(dim white)- %an%C(reset)' --all";
        lg = "!\"git lg1\"";
        pr = "!\"git fetch origin pull/$1/head:pr-$1; git co pr-$1 #\"";
      };

      ignores = [
        ".DS_Store"
        ".envrc"
        ".cache"
        "dist-newstyle"
        "cabal.project.local"
        "*.hi"
        "*.ho"
        "*.pyc"
        "*.pyo"
      ];

      extraConfig = {
        core = {
          editor = "vim";
          pager = "less";
          pull.rebase = true;
        };
        commit.gpgsign = true;
        color.ui = "auto";
        merge.tool = "opendiff";
        "url \"git@github.com:\"".insteadOf = "https://github.com/";
      };

    };

  };

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  #services.nix-daemon.enable = true;
  #nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = false;
  programs.fish.enable = false;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
