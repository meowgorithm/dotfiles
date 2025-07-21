{
  pkgs,
  inputs,
  ...
}: let
  buildVimPlugin = name:
    pkgs.vimUtils.buildVimPlugin {
      name = name;
      src = inputs."${name}";
    };
in {
  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      ale
      copilot-vim
      ctrlp-vim
      vim-commentary
      vim-fern
      vim-fugitive
      vim-gitgutter
      vim-gnupg
      vim-surround
      vim-unimpaired
      (buildVimPlugin "vim-bufonly")
      (buildVimPlugin "vim-healthcheck")
    ];
    extraConfig = builtins.readFile ./vimrc;
  };
}
