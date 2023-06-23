lib: isNvim: let
  inherit (lib) concatStrings mapAttrsToList;
  inherit (builtins) concatStringsSep;

  mkReset = name: {...}: "hi ${name} guifg=NONE guibg=NONE gui=NONE cterm=NONE";

  mkRule = name: {
    fg ? "NONE",
    bg ? "NONE",
    gui ? ["NONE"],
  }: let
    guiStr = concatStringsSep "," gui;
  in "hi ${name} guifg=${fg} guibg=${bg} gui=${guiStr} cterm=${guiStr}";

  builtin.fg = "#ef8d34";
  comment.fg = "#676767";
  constant.fg = "#00ccbb";
  declaration.fg = "#1dc967";
  function.fg = "#0baef4";
  keyword.fg = "#dc59de";
  operator.fg = "#ec6965";
  punctuation.fg = "#c2c99c";
  specialChar.fg = "#8352FF";
  string.fg = "#c69669";
  type.fg = "#835f5e";
  functionName.fg = "#ffffff";
  include.fg = "#f64c8d";

  ui = {
    ColorColumn.bg = "#202020";
    CursorLine.bg = "#090909";
    LineNr.fg = "#4d4d4d";
    Normal.fg = "#C5C8C6";
    Search = {
      fg = "#202020";
      bg = "#dcf764";
    };
    Visual.bg = "#4128bd";
    CursorLineNr.fg = "#8c8c8c";
    Folded = {
      fg = "#767676";
      bg = "#2a2a2a";
    };
    PMenu = {
      fg = "#a8a8a8";
      bg = "#090909";
    };
    NonText.fg = "#414141";
    PMenuSel = {
      fg = "#c2a3e0";
      bg = "#5f00ff";
    };
    PMenuSBar.bg = "#005f5f";
    PMenuThumb.bg = "#00af87";
    SignColumn = {};
    VertSplit.fg = "#333333";
  };

  defaults = {
    Comment = {};

    Constant = {};
    String = {};
    Character = {};
    Number = {};
    Boolean = {};
    Float = {};

    Identifier = {}; # variable names
    Function = {};

    Statement = declaration;
    Conditional = keyword; # if, then, else, switch, etc.
    Repeat = keyword; # for, do, while, etc.
    Label = keyword; # case, default, etc.
    Operator = operator;
    Keyword = keyword; # any other keyword
    Exception = keyword; # try, catch, throw

    PreProc = builtin;
    Include = {};
    Define = {};
    Macro = {};
    PreConduit = {}; # preprocessor keywords: #if, #else, etc;

    Type = type;
    StorageClass = {};
    Structure = {};
    Typedef = {};

    Special = specialChar;
    SpecialChar = specialChar;
    Tag = {};
    Delimiter = punctuation;
    SpecialComment = specialChar;
    Debug = {};

    #Underlined = {};
    #Ignore = {};
    #Error = {};
    #Todo = {};
  };

  go =
    if isNvim
    then {
      # Treesitter
      "@constant.go" = constant;
      "@boolean.go" = constant;
      "@number.go" = constant;

      "@label.go" = {};

      "@keyword.go" = keyword;
      "@conditional.go" = keyword;
      "@keyword.return.go" = keyword;
      "@keyword.coroutine.go" = keyword;
      "@keyword.function.go" = declaration;

      "@type.go" = type;
      "@type.builtin.go" = builtin;
      "@parameter.go" = constant;
      "@field.go" = specialChar;

      "@method.go" = functionName;
      "@include.go" = include;

      "@function.builtin.go" = builtin;
      "@function.call.go" = function;
      "@method.call.go" = function;

      "@punctuation.bracket.go" = punctuation;
      "@punctuation.delimiter.go" = punctuation;
      "@operator.go" = operator;

      "@string.go" = string;
      "@string.escape.go" = specialChar;

      "@comment.go" = comment;
    }
    else {
      # vim-go
      goPackage = include;
      goImport = include;
      goParen = punctuation;
      goBlock = {};
      goType = type;
      goParamType = type;
      goDeclType = type;
      goStatement = keyword;
      goLabel = keyword;
      goDeclaration = declaration;
      goFunction = functionName;
      goTypeName = functionName;
      goBuiltins = builtin;
      goReceiver = punctuation;
      goReceiverVar = constant;
      goPointerOperator = operator;
      goOperator = operator;
      goParamName = constant;
      goSimpleParams = punctuation;
      goFunctionCall = function;
      goConst = keyword;
    };

  mkRules = rules:
    "\" Reset.\n"
    + (concatStringsSep "\n" (mapAttrsToList mkReset rules))
    + "\n\n\" Rules.\n"
    + (concatStringsSep "\n" (mapAttrsToList mkRule rules))
    + "\n\n";
in
  concatStrings (map mkRules [ui defaults go css])
