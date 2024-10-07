{
  pkgs,
  inputs,
  ...
}: let
  colorscheme = (import ./colorscheme.nix) pkgs.lib;

  buildVimPlugin = name:
    pkgs.vimUtils.buildVimPlugin {
      name = name;
      src = inputs."${name}";
    };
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
      nvim-colorizer-lua
      nvim-code-action-menu
      nvim-cmp
      nvim-lspconfig
      nvim-tree-lua
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
      telescope-nvim
      trouble-nvim
      vim-airline
      vim-gitgutter
      vim-gnupg
      vim-vsnip
      xterm-color-table-vim

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
