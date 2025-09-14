{pkgs, ...}: let
  colorscheme = (import ./colorscheme.nix) pkgs.lib;
in {
  imports = [
    ./vsnip.nix
  ];

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withPython3 = true;
    plugins = with pkgs.vimPlugins; [
      # LSP-ish
      blink-cmp
      nvim-lspconfig
      conform-nvim

      BufOnly-vim
      copilot-vim
      nvim-colorizer-lua
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
            templ
            typescript
          ]
      ))
    ];
    extraLuaConfig = ''
      ${builtins.readFile ./init.lua}
      vim.cmd 'colorscheme pantera-negra'
    '';
  };

  xdg.configFile."nvim/colors/pantera-negra.vim".text = colorscheme true;
}
