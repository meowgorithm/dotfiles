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
      nvim-cmp
      nvim-lspconfig
      nvim-treesitter
      trouble-nvim
      vim-airline
      vim-gitgutter
      vim-gnupg
      vim-vsnip
    ];
    extraLuaConfig = builtins.readFile ./init.lua;
  };
}
