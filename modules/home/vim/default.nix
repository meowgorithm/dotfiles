{pkgs, ...}: let
  colorscheme = (import ./colorscheme.nix) pkgs.lib;
in {
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
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
      goyo-vim
      null-ls-nvim
      nvim-cmp
      nvim-code-action-menu
      nvim-colorizer-lua
      nvim-lspconfig
      nvim-tree-lua
      telescope-nvim
      trouble-nvim
      vim-airline
      vim-fugitive
      vim-gitgutter
      vim-gnupg
      vim-surround
      vim-unimpaired
      vim-vsnip
      xterm-color-table-vim

      (nvim-treesitter.withPlugins (
        plugins:
          with plugins; [
            nix
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

      # Avante and its dependencies
      avante-nvim
      dressing-nvim
      img-clip-nvim # keeps avante from complaining
      nui-nvim
      plenary-nvim
      render-markdown-nvim
    ];
    extraLuaConfig = ''
      ${builtins.readFile ./init.lua}
      vim.cmd 'colorscheme pantera-negra'
    '';
  };

  xdg.configFile."nvim/colors/pantera-negra.vim".text = colorscheme true;
}
