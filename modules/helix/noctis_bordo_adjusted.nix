let
  base00 = "#322a2d"; # Default Background
  base01 = "#2c2528"; # Lighter Background (Used for status bars, line number and folding marks)
  base02 = "#997582"; # Selection Background
  base03 = "#585858"; # Comments, Invisibles, Line Highlighting
  base04 = "#322a2d"; # Dark Foreground (Used for status bars)
  base05 = "#cbbec2"; # Default Foreground, Caret, Delimiters, Operators
  base06 = "#e8e8e8"; # Light Foreground (Not often used)
  base07 = "#f8f8f8"; # Light Background (Not often used)
  base08 = "#e4b781"; # Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
  base09 = "#d5971a"; # Integers, Boolean, Constants, XML Attributes, Markup Link Url
  base0A = "#df769b"; # Classes, Markup Bold, Search Text Background
  base0B = "#49e9a6"; # Strings, Inherited Class, Markup Code, Diff Inserted
  base0C = "#16b673"; # Support, Regular Expressions, Escape Characters, Markup Quotes
  base0D = "#16a3b6"; # Functions, Methods, Attribute IDs, Headings
  base0E = "#ba8baf"; # Keywords, Storage, Selector, Markup Italic, Diff Changed
  base0F = "#d67e5c"; # Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
  base10 = "#b0b0ff"; # Types
in {
  "attributes" = {
    fg = "#7060eb";
    modifiers = ["bold"];
  };
  "comment" = {
    fg = base03;
    modifiers = ["italic"];
  };
  "comment.block.documentation" = {
    fg = base06;
    modifiers = ["italic"];
  };
  "constant" = base09;
  "constant.character.escape" = base0C;
  "constant.numeric" = "#7060eb";
  "constructor" = base0D;
  "function" = base0D;
  "keyword" = base0E;
  "keyword.control" = {
    fg = base0E;
    modifiers = ["bold"];
  };
  "keyword.directive" = "white";
  "keyword.import" = {fg = "#df769b";};
  "keyword.operator" = {
    fg = "base0E";
    modifiers = ["italic"];
  };
  "label" = base0E;
  "namespace" = base0E;
  "operator" = base05;
  "string" = base0B;
  "type" = base10;
  "variable" = base08;
  "variable.other.member" = base08;
  "special" = base0D;

  "ui.background" = {bg = base00;};
  "ui.virtual" = base03;
  "ui.menu" = {
    fg = base05;
    bg = base01;
  };
  "ui.menu.selected" = {
    fg = base0B;
    bg = base01;
  };
  "ui.popup" = {bg = base01;};
  "ui.window" = {bg = base01;};
  "ui.linenr" = {
    fg = "#715b63";
  };
  "ui.linenr.selected" = {
    fg = base02;
    modifiers = ["bold"];
  };
  "ui.selection" = {
    fg = base05;
    bg = base02;
  };
  "ui.statusline" = {
    fg = base02;
    bg = base01;
  };
  "ui.cursor" = {
    fg = base04;
    modifiers = ["reversed"];
  };
  "ui.cursor.primary" = {
    fg = base05;
    modifiers = ["reversed"];
  };
  "ui.text" = base02;
  "ui.text.focus" = base05;
  "ui.cursor.match" = {
    fg = base0A;
    modifiers = ["underlined"];
  };
  "ui.cursorline.primary" = {bg = base01;};
  "ui.help" = {
    fg = base06;
    bg = base01;
  };
}
