let
  colors = import ../colors.nix;
  normal = colors.normal;
  bright = colors.bright;
in {
  "ui.window" = normal.black;
  "ui.menu" = {bg = normal.black;};
  "ui.menu.selected" = {bg = bright.black;};
  "ui.menu.scroll" = {
    fg = normal.black;
    bg = normal.black;
  };
  "ui.linenr" = {
    fg = "#3A3A3A";
  };
  "ui.linenr.selected" = {
    fg = "#6C6C6C";
  };
  "ui.popup" = {
    fg = normal.white;
    bg = normal.black;
  };
  "ui.popup.info" = {
    fg = normal.white;
  };
  "ui.selection" = {
    fg = normal.black;
    bg = normal.blue;
  };
  "ui.selection.primary" = {modifiers = ["reversed"];};
  "ui.statusline" = {
    fg = normal.white;
  };
  "ui.statusline.inactive" = {
    fg = normal.black;
  };
  "ui.help" = {
    fg = normal.white;
  };
  "ui.cursor" = {modifiers = ["reversed"];};
  "ui.cursorline.primary" = {bg = "#080808";};
  "ui.cursorline.secondary" = {bg = "#121212";};
  "ui.virtual.ruler" = {bg = "#1D1D1D";};
  "ui.virtual.whitespace" = {fg = "#3A3A3A";};
  "ui.cursor.match" = {
    fg = normal.cyan;
    modifiers = ["underlined"];
  };
  "ui.gutter" = {};

  "warning" = normal.yellow;
  "hint" = normal.white;
  "info" = normal.blue;

  "diff.delta" = normal.yellow;
  "diff.minus" = normal.red;
  "diff.plus" = normal.green;

  "attributes" = normal.blue;
  "comment" = {fg = bright.black;};
  "constant" = normal.yellow;
  "constant.character.escape" = normal.yellow;
  "constant.numeric" = normal.yellow;
  "constructor" = normal.blue;
  "debug" = normal.white;
  "diagnostic" = {modifiers = ["underlined"];};
  "error" = normal.red;
  "function" = normal.yellow;
  "function.builtin" = normal.blue;
  "function.method" = normal.blue;
  "keyword" = bright.magenta;
  "keyword.control.repeat" = normal.magenta;
  "label" = normal.magenta;
  "namespace" = normal.blue;
  "special" = normal.yellow;
  "string" = normal.green;
  "type" = normal.cyan;
  "variable" = normal.white;
  "variable.builtin" = normal.yellow;
  "variable.other.member" = normal.blue;
}
