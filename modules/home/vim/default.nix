{
  pkgs,
  inputs,
  ...
}: let
  colorscheme = (import ./colorscheme.nix) pkgs.lib;
in {
  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      BufOnly-vim
      ale
      asyncomplete-buffer-vim
      asyncomplete-file-vim
      asyncomplete-lsp-vim
      asyncomplete-vim
      copilot-vim
      fern-vim
      fzf-vim
      neoformat
      vim-airline
      vim-commentary
      vim-easy-align
      vim-gitgutter
      vim-gnupg
      vim-lsp
      vim-lsp-settings
      vim-sensible
    ];
    extraConfig = ''
      if !has('vim9script')
        finish
      endif

      source $HOME/.vim/vimrc
      colorscheme x
    '';
  };
  home.file.".vim/vimrc".source = ./vimrc;
  home.file.".vim/colors/x.vim".text = colorscheme;

  programs.neovim = {
    enable = true;
    viAlias = false;
    vimAlias = false;
    vimdiffAlias = true;
    withNodeJs = true;
    withPython3 = true;
    plugins = with pkgs.vimPlugins; [
      BufOnly-vim
      cmp-buffer
      cmp-cmdline
      cmp-nvim-lsp
      cmp-path
      cmp-vsnip
      copilot-vim
      null-ls-nvim
      nvim-colorizer-lua
      nvim-code-action-menu
      nvim-cmp
      nvim-lspconfig
      nvim-tree-lua
      (nvim-treesitter.withPlugins (
        plugins:
          with plugins; [
            bash
            go
            gomod
            gowork
            haskell
          ]
      ))
      telescope-nvim
      trouble-nvim
      vim-airline
      vim-commentary
      vim-gitgutter
      vim-gnupg
      vim-vsnip
      (
        pkgs.vimUtils.buildVimPluginFrom2Nix {
          name = "color-picker-nvim";
          src = inputs.color-picker-nvim;
        }
      )
    ];
    extraLuaConfig = ''
      ${builtins.readFile ./init.lua}
      vim.cmd 'colorscheme x'
    '';
  };

  xdg.configFile."nvim/colors/x.vim".text = colorscheme;
}
