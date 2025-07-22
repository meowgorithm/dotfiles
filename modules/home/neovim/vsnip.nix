{
  config,
  lib,
  pkgs,
  ...
}: let
  snippets = {
    go = {
      Function = {
        prefix = "fn";
        body = [
          "func \${1:name}(\${2:args}) \${3:returnType} {\n\t\${0}\n}"
        ];
      };
    };

    haskell = {
      "Language Extension" = {
        prefix = "#";
        body = "{-# LANGUAGE \${1:Extension} #-}";
      };
      LambdaCase = {
        prefix = "lc";
        body = "{-# LANGUAGE LambdaCase #-}";
      };
      OverloadedStrings = {
        prefix = "os";
        body = "{-# LANGUAGE OverloadedStrings #-}";
      };
      "Import ByteString" = {
        prefix = "ibs";
        body = "import qualified Data.ByteString as BS";
      };
      "Import Lazy ByteString" = {
        prefix = "ilbs";
        body = "import qualified Data.ByteString.Lazy as LBS";
      };
      "Import Text Conversions" = {
        prefix = "itc";
        body = "import Data.Text.Conversions (convertText)";
      };
    };

    scss = {
      Color = {
        prefix = "c";
        body = ["color: \${1:red};"];
      };
      "Background Color" = {
        prefix = "bgc";
        body = "background-color: \${1:red};";
      };
      Padding = {
        prefix = "p";
        body = "padding: \${1:0};";
      };
      Margin = {
        prefix = "m";
        body = "margin: \${1:0};";
      };
      "Font Size" = {
        prefix = "fs";
        body = "font-size: \${1:1rem};";
      };
      "Border Radius" = {
        prefix = "br";
        body = "border-radius: \${1:0};";
      };
    };
  };

  snipFile = lang: "${config.home.homeDirectory}/.vsnip/${lang}.json";

  # Generate home.file entries for all snippets with pretty printing.
  snippetFiles = lib.mapAttrs' (lang: snippetDefs:
    lib.nameValuePair
    (snipFile lang)
    {
      # Pretty print the JSON output.
      text = builtins.readFile (pkgs.runCommand "${lang}-snippets.json" {} ''
        echo '${builtins.toJSON snippetDefs}' | ${pkgs.jq}/bin/jq . > $out
      '');
    })
  snippets;
in {
  home.file = snippetFiles;
}
