lib: let
  inherit (builtins) concatStringsSep;
  inherit (lib) mapAttrsToList;

  rule = name: {
    fg ? "NONE",
    bg ? "NONE",
    gui ? "NONE",
  }: "hi ${name} guifg=${fg} guibg=${bg} gui=${gui}";
in
  concatStringsSep "\n" (mapAttrsToList rule {
    SignColumn.bg = "none";
    VertSplit = {
      fg = "#262626";
      bg = "#585858";
    };
    Comment.fg = "#565656";
    String.fg = "#B18A6D";
  })
