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
      # LSP
      cmp-buffer
      cmp-cmdline
      cmp-nvim-lsp
      cmp-path
      cmp-vsnip
      null-ls-nvim
      nvim-cmp
      nvim-code-action-menu
      nvim-lspconfig

      nvim-colorizer-lua
      BufOnly-vim
      copilot-vim
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
            typescript
          ]
      ))

      # Avante and its dependencies
      avante-nvim
      dressing-nvim
      nui-nvim
      plenary-nvim

      # Plugins not in nixpkgs
      (buildVimPlugin "smear")
    ];
    extraLuaConfig = ''
      ${builtins.readFile ./init.lua}
      vim.cmd 'colorscheme pantera-negra'
    '';
  };

  xdg.configFile."nvim/colors/pantera-negra.vim".text = colorscheme true;
}
