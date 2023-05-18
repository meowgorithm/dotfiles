{pkgs, ...}: {
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
      ctrlp-vim
      nerdcommenter
      nerdtree
      null-ls-nvim
      nvim-code-action-menu
      nvim-cmp
      nvim-lspconfig
      nvim-treesitter
      trouble-nvim
      vim-airline
      pkgs.vim-colortemplate
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
