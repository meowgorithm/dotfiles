pkgs: {
  programs.git = {
    enable = true;
    userName = "Christian Rocha";
    userEmail = "christian@rocha.is";

    ignores = [
      ".DS_Store"
      ".ghc.environment.*"
      "xmonad-x86_64-linux"
      "xmonad.errors"
      "*.hi"
      "*.o"
    ];

    signing = {
      signByDefault = true;
      key = "92B3C5E83E309FE64CB2F833589F6FDD5B820611";
    };

    aliases = {
      st = "status";
      ci = "commit";
      co = "checkout";
      count = "!git log --pretty=oneline | wc -l";
      sync = "fetch --all --tags --prune";
      housekeeping = "!git fsck && git fsck --unreachable && git gc --aggressive --prune && git prune && git prune-packed";
      lg1 = "log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all";
      lg2 = "log --graph --format=format:'%C(117)%h%C(reset) %s %C(dim white)%ar%C(reset) %C(181)%ae%C(reset) %C(141)%d%C(reset)' --all";
      lgs = "log --graph --format=format:'%C(117)%h%C(reset) %C(226)%G?%C(reset) %s %C(dim white)%ar%C(reset) %C(181)%ae%C(reset) %C(141)%d%C(reset)' --all";
      lg = "!git lg1";
      quickfix = "rebase --interactive HEAD^^";
      pr = "!gh pr list | cut -f1,2 | gum choose | cut -f1 | xargs gh pr checkout";
    };

    extraConfig = {
      color.ui = true;
      core = {
        editor = "hx";
      };
    };
  };
}
