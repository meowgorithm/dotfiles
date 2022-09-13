{
  config,
  pkgs,
  lib,
  ...
} @ inputs: let
  note = name: "\n\n# --- ${name} ---\n\n";
in {
  programs.bash = {
    enable = true;
    enableCompletion = true;
    historyControl = ["ignoredups" "erasedups"];
    historyIgnore = ["ls" "cd" "exit"];
    sessionVariables = {
      EDITOR = "kak";
      PROMPT_DIRTRIM = "2";
      XDG_DATA_DIRS = "$HOME/.nix-profile/share:$XDG_DATA_DIRS"; # necessary for completion
      GIT_PS1_SHOWDIRTYSTATE = "true";
      GIT_PS1_SHOWUNTRACKEDFILES = "true";
      GIT_PS1_SHOWSTASHSTATE = "true";
    };
    shellOptions = ["histappend"];
    shellAliases = {};
    initExtra =
      note "git-prompt"
      + "source ${inputs.pkgs.git}/share/git/contrib/completion/git-prompt.sh"
      + note "bash funcs"
      + builtins.readFile ./bash_funcs
      + note ".bashrc"
      + builtins.readFile ./bashrc
      + note "end extra";
  };
}
