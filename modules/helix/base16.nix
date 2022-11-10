let
  colors = import ../colors.nix;
  normal = colors.normal;
  bright = colors.bright;
in {
  "ui.menu" = {bg = normal.black;};
  "ui.menu.selected" = {bg = bright.black;};
  "ui.menu.scroll" = {
    fg = normal.black;
    bg = normal.black;
  };
  "ui.window" = normal.black;
  "ui.linenr" = {
    fg = normal.black;
  };
  "ui.linenr.selected" = {
    fg = normal.white;
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
  "comment" = {fg = bright.black;};
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
  "ui.virtual.ruler" = {bg = normal.black;};
  "variable" = normal.white;
  "variable.builtin" = normal.yellow;
  "constant.numeric" = normal.yellow;
  "constant" = normal.yellow;
  "attributes" = normal.blue;
  "type" = normal.cyan;
  "ui.cursor.match" = {
    fg = normal.cyan;
    modifiers = ["underlined"];
  };
  "string" = normal.green;
  "variable.other.member" = normal.blue;
  "constant.character.escape" = normal.yellow;
  "function" = normal.yellow;
  "function.builtin" = normal.blue;
  "function.method" = normal.blue;
  "constructor" = normal.blue;
  "special" = normal.yellow;
  "keyword" = bright.magenta;
  "keyword.control.repeat" = normal.magenta;
  "label" = normal.magenta;
  "namespace" = normal.blue;
  "diff.plus" = normal.green;
  "diff.delta" = normal.yellow;
  "diff.minus" = normal.red;
  "diagnostic" = {modifiers = ["underlined"];};
  "ui.gutter" = {};
  "info" = normal.blue;
  "hint" = normal.white;
  "debug" = normal.white;
  "warning" = normal.yellow;
  "error" = normal.red;
}
