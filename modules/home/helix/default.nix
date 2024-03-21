{...}: {
  xdg.configFile."helix/ignore".text = ''
    *.gif
    *.mp4
    *.webm
  '';

  programs.helix = {
    enable = true;

    settings = {
      theme = "charm";
      keys.normal = {
        g.q = ":reflow";
        g.w = ":set whitespace.render all";
        g.W = ":set whitespace.render none";
        g.F.o = ":set auto-format false";
        g.F.O = ":set auto-format true";
        g.R = ":reload-all";
        g.C = ":buffer-close-others";
        X = "extend_line_above";
        minus = "goto_prev_paragraph";
        "=" = "goto_next_paragraph";
        D = ["select_mode" "goto_line_end" "delete_selection" "normal_mode"];
        space.w = ":write";
        space.q = ":quit";
      };
      editor = {
        cursorline = true;
        true-color = true;
        lsp.display-messages = true;
        lsp.display-inlay-hints = true;
        rulers = [80];
        auto-format = true;
        color-modes = true;
        statusline = {
          mode.normal = "NORMAL";
          mode.insert = "INSERT";
          mode.select = "SELECT";
          left = ["mode" "spinner" "file-name"];
          center = [];
          right = ["diagnostics" "selections" "position" "file-encoding" "file-line-ending" "file-type" "version-control"];
        };
      };
    };

    languages.language = [
      {
        name = "nix";
        auto-format = true;
        formatter = {command = "alejandra";};
        language-servers = ["nil"];
      }
      {
        name = "haskell";
        auto-format = true;
      }
      {
        name = "cabal";
        scope = "";
        roots = [];
        file-types = ["cabal"];
        formatter = {command = "cabal-fmt";};
        auto-format = true;
      }
      {
        name = "go";
        formatter = {command = "goimports";};
      }
      {
        name = "lua";
        auto-format = true;
      }
      {
        name = "html";
        indent = {
          tab-width = 2;
          unit = " ";
        };
        auto-format = false;
        formatter = {
          command = "prettier";
          args = ["--parser" "html" "--tab-width" "2"];
        };
      }
      {
        name = "css";
        indent = {
          tab-width = 4;
          unit = " ";
        };
        formatter = {
          command = "prettier";
          args = ["--parser" "css" "--tab-width" "2"];
        };
      }
      {
        name = "typescript";
        indent = {
          tab-width = 4;
          unit = " ";
        };
        auto-format = true;
        formatter = {
          command = "prettier";
          args = ["--parser" "typescript" "--tab-width" "4"];
        };
      }
      {
        name = "svg";
        scope = "";
        roots = [];
        file-types = ["svg"];
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
