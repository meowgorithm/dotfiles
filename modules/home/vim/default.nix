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
      vim-airline
      vim-commentary
      vim-css-color
      vim-easy-align
      vim-gitgutter
      vim-gnupg
      vim-go
      vim-lsp
      vim-lsp-settings
      vim-sensible
    ];
    extraConfig = ''
      if !has('vim9script')
        finish
      endif
      source $HOME/.vim/vimrc
      colorscheme pantera-negra
    '';
  };
  home.file.".vim/vimrc".source = ./vimrc;
  home.file.".vim/colors/pantera-negra.vim".source = ./pantera-negra.vim;
  home.file.".vim/colors/x.vim".text = colorscheme false;

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
            css
            elm
            go
            gomod
            gowork
            haskell
            html
            javascript
            json
            lua
            scss
            typescript
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
      vim.cmd 'colorscheme pantera-negra'
    '';
  };

  xdg.configFile."nvim/colors/pantera-negra.vim".source = ./pantera-negra.vim;
  xdg.configFile."nvim/colors/x.vim".text = colorscheme true;
}
