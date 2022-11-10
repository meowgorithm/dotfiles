helixPackage: {
  config,
  pkgs,
  lib,
  helix,
  ...
}: {
  programs.helix = {
    enable = true;
    package = helixPackage;

    settings = {
      theme = "charm";
      keys.normal = {
        g.q = ":reflow";
        X = "extend_line_above";
        "minus" = "goto_prev_paragraph";
        "=" = "goto_next_paragraph";
      };
      editor = {
        cursorline = true;
        true-color = true;
        cursor-shape.insert = "bar";
        lsp.display-messages = true;
        rulers = [80];
      };
    };

    languages = [
      {
        name = "nix";
        auto-format = true;
        formatter = {command = "alejandra";};
      }
      {
        name = "cabal";
        scope = "";
        roots = [];
        file-types = ["cabal"];
        auto-format = true;
        formatter = {command = "cabal-fmt";};
      }
      {
        name = "scss";
        language-server = {
          command = "css-languageserver";
          args = ["--stdio"];
        };
      }
      {
        name = "css";
        language-server = {
          command = "css-languageserver";
          args = ["--stdio"];
        };
      }
    ];

    themes = {
      charm = import ./base16.nix;
    };
  };
}
