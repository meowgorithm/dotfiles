{...}: {
  xdg.configFile."elvish/rc.elv".text = ''
    set-env CARAPACE_BRIDGES 'zsh,fish,bash,inshellisense' # optional
    eval (carapace _carapace|slurp)
  '';
}
