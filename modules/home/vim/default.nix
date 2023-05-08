{pkgs, ...}: {
  programs.neovim = {
    enable = true;
    withNodeJs = true;
    withPython3 = true;
    plugins = with pkgs.vimPlugins; [
      BufOnly-vim
      cmp-buffer
      cmp-nvim-lsp
      cmp-path
      ctrlp-vim
      nerdcommenter
      nerdtree
      nvim-cmp
      nvim-lspconfig
      nvim-treesitter
      null-ls-nvim
      trouble-nvim
      vim-airline
      vim-gitgutter
      vim-gnupg
      which-key-nvim
    ];
    extraLuaConfig = ''
      ${builtins.readFile ./init.lua};
      ${builtins.readFile ./lsp.lua};
    '';
  };
}
