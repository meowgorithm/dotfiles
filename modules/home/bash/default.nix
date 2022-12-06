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
  programs.bash = {
    enable = true;
    enableCompletion = true;
    historyControl = ["ignoredups" "erasedups"];
    historyIgnore = ["ls" "cd" "exit"];
    sessionVariables =
      {
        EDITOR = "hx";
        PROMPT_DIRTRIM = "2";
        XDG_DATA_DIRS = "$HOME/.nix-profile/share:$XDG_DATA_DIRS"; # necessary for completion
        GIT_PS1_SHOWDIRTYSTATE = "true";
        GIT_PS1_SHOWUNTRACKEDFILES = "true";
        GIT_PS1_SHOWSTASHSTATE = "true";
      }
      // (
        if pkgs.stdenv.isDarwin
        then {}
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

  # Also put our funcs here so we can source them.
  xdg.dataFile."meowgorithm/bash_funcs".text = builtins.readFile ./bash_funcs;
}
