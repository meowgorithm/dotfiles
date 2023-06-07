{pkgs, ...}: {
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
      null-ls-nvim
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
    ];
    extraLuaConfig = ''
      vim.cmd([[
      ${(import ./colorscheme.nix) pkgs.lib}
      ]])
      ${builtins.readFile ./init.lua}
    '';
  };
}
