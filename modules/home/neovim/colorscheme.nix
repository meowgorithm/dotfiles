lib: let
  inherit (lib) concatStrings mapAttrsToList;

  rule = name: {
    fg ? "",
    bg ? "",
    gui ? "",
  }:
    if fg == "" && bg == "" && gui == ""
    then ""
    else
      (
        "hi ${name}"
        + (
          if fg == ""
          then ""
          else " guifg=${fg}"
        )
        + (
          if bg == ""
          then ""
          else " guibg=${bg}"
        )
        + (
          if gui == ""
          then ""
          else " gui=${gui}"
        )
        + "\n"
      );

  bold = "bold";
  underline = "underline";
  none = "none";
in
  concatStrings (mapAttrsToList rule {
    ColorColumn.bg = "#202020";
    CursorLine.bg = "#090909";
    CursorLineNr = {};
    Folded = {
      fg = "#767676";
      bg = "#2a2a2a";
    };
    LineNr.fg = "#505050";
    NonText.fg = "#414141";
    PMenu.bg = "#333333";
    SignColumn.bg = none;
    VertSplit = {
      fg = "#262626";
      bg = "#585858";
    };

    Normal = {};

    Boolean = {};
    Comment = {};
    Conditional = {};
    Constant = {gui = bold;};
    Debug = {};
    Define = {};
    Delimiter = {};
    Exception = {};
    Float = {};
    Function = {};
    Identifier = {};
    Include = {};
    Keyword = {};
    Label = {};
    Macro = {};
    Operator = {};
    PreConduit = {};
    PreProc = {};
    Repeat = {};
    Special = {};
    SpecialComment = {};
    Statement = {};
    StorageClass = {};
    String = {};
    Structure = {};
    Tag = {};
    Type = {};
    Typedef = {};

    Error = {};
    Hint = {};
    Information = {};
    Warning = {};

    DiffDelete = {};
    ErrorMsg = {};

    GitSignsAdd = {};
    GitSignsChange = {};
    GitSignsDelete = {};
  })
