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
  "ui.selection.primary" = {
    fg = "#FFFDF5";
    bg = "#5C42FF";
  };
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
  "diff.minus" = "#FD5B5B";
  "diff.plus" = "#00D787";

  "constructor" = normal.blue;
  "debug" = normal.white;
  "diagnostic" = {modifiers = ["underlined"];};
  "error" = {
    fg = "#F1F1F1";
    bg = "#F05B5B";
  };
  "label" = "#C69669";

  "attribute" = "#FD5B5B"; # class attributes, HTML tag attributes
  "type" = "#6E6ED8";
  "constant" = "#FFFF87";
  "constant.character.escape" = "#AFFFD7";
  "constant.numeric" = "#FFFF87";
  "constant.numeric.integer" = "#FFFF87";
  "constant.numeric.float" = "#FFFF87";
  "constant.builtin" = ""; # constants proviced by the language
  "constant.builtin.boolean" = "";
  "string" = "#C69669";
  "string.regexp" = "#C69669";
  "string.special" = "#FF5FD2";
  "string.special.path" = "#C69669";
  "string.special.url" = "#C69669";
  "string.special.symbol" = "#C69669"; # erlang atoms, ruby symbols, clojure keywords
  "comment.line" = "#676767";
  "comment.block" = "#676767";
  "comment.block.documentation" = "#676767";
  "variable" = normal.white;
  "variable.builtin" = "#FF5FD2"; # self, this, super, etc
  "variable.parameter" = normal.white; # function parameters
  "variable.other.member" = normal.blue; # struct fields, unions

  "keyword" = "#00AAFF";
  "keyword.control.conditional" = "#00AAFF";
  "keyword.control.import" = "#00AAFF";
  "keyword.control.return" = "#00AAFF";
  "keyword.control.exception" = "#00AAFF";
  "keyword.operator" = "#00AAFF"; # or, in
  "keyword.directive" = "#FF875F"; # preprocessor directives
  "keyword.storage" = "#FF875F";
  "keyword.storage.type" = "#FF875F"; # class, function, var, let, etc
  "keyword.storage.modifier" = "#FF875F"; # static, mut, const, ref, etc
  "operator" = "#EF8080";
  "function" = "#00D787";
  "function.builtin" = "#FF8EC7";
  "function.method" = "#FD5B5B";
  "function.macro" = "#FD5B5B";
  "function.special" = "#FD5B5B";
  "tag" = "";
  "namespace" = "#FF5F87";
}
