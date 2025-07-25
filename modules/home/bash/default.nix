{
  config,
  pkgs,
  lib,
  ...
} @ inputs: let
  note = name: "\n\n# --- ${name} ---\n\n";

  readIntoVar = varName: path:
    "rc=\"$(cat <<EOF\n"
    + builtins.readFile path
    + "EOF\n"
    + ")\"\n\n";
in {
  home.packages = with pkgs; [
    bashInteractive
  ];

  programs.bash = {
    enable = true;
    enableCompletion = true;
    sessionVariables =
      {
        EDITOR = "nvim";
        XDG_DATA_DIRS = "$HOME/.nix-profile/share:$XDG_DATA_DIRS"; # necessary for completion
        XDG_DATA_HOME = "$HOME/.local/share";
        GIT_PS1_SHOWDIRTYSTATE = "true";
        GIT_PS1_SHOWUNTRACKEDFILES = "true";
        GIT_PS1_SHOWSTASHSTATE = "true";
        POP_DEFAULT_FROM = "christian@rocha.is";
      }
      // (
        if pkgs.stdenv.isDarwin
        then {
          HOMEBREW_NO_ANALYTICS = "1";
        }
        else {
          HARDWARECLOCK = "localtime";
          BASH_ENV = "$HOME/.bashrc";
        }
      );
    shellOptions = ["histappend"];
    shellAliases = {};
    initExtra =
      note "git-prompt"
      + "source ${inputs.pkgs.git}/share/git/contrib/completion/git-prompt.sh"
      + note ".bashrc"
      + builtins.readFile ./bashrc
      + note "rc"
      + readIntoVar "rc" ./rc.gpg
      + "decryptAndSource \"$rc\""
      + note "end extra";
    bashrcExtra =
      note "bash_funcs"
      + builtins.readFile ./bash_funcs;
  };

  # Also put our funcs here so we can source them in scripts.
  xdg.dataFile."meowgorithm/bash_funcs".text = builtins.readFile ./bash_funcs;
}
