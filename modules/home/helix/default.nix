{
  config,
  pkgs,
  lib,
  helix,
  ...
}: {
  programs.helix = {
    enable = true;

    settings = {
      theme = "charm";
      keys.normal = {
        g.q = ":reflow";
        g.w = ":set whitespace.render all";
        g.W = ":set whitespace.render none";
        X = "extend_line_above";
        "minus" = "goto_prev_paragraph";
        "=" = "goto_next_paragraph";
      };
      editor = {
        cursorline = true;
        true-color = true;
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
        name = "go";
        formatter = {command = "goimports";};
      }
      {
        name = "html";
        language-server = {
          command = "html-languageserver";
          args = ["--stdio"];
        };
        auto-format = false;
        formatter = {
          command = "prettier";
          args = ["--parser" "html" "--tab-width" "2"];
        };
      }
      {
        name = "css";
        language-server = {
          command = "css-languageserver";
          args = ["--stdio"];
        };
        auto-format = false;
        formatter = {
          command = "prettier";
          args = ["--parser" "css" "--tab-width" "4"];
        };
      }
      {
        name = "typescript";
        indent = {
          tab-width = 4;
          unit = " ";
        };
        formatter = {
          command = "prettier";
          args = ["--parser" "typescript" "--tab-width" "4"];
        };
        auto-format = true;
      }
      {
        name = "svg";
        scope = "";
        roots = [];
        file-types = ["svg"];
        auto-format = true;
        formatter = {
          command = "svgo";
          args = ["--pretty" "-"];
        };
      }
    ];

    themes = {
      charm = import ./charm.nix;
    };
  };
}