{...}: {
  programs.ssh = {
    enable = true;
    includes = ["~/.ssh/config.local"];

    matchBlocks = {
      "*" = {
        identityFile = "~/.ssh/id_ed25519";
        setEnv = {
          TERM = "xterm-256color";
        };

        sendEnv = ["COLORTERM"];
      };

      "localhost" = {
        extraOptions = {
          UserKnownHostsFile = "/dev/null";
          StrictHostKeyChecking = "no";
        };
      };
    };
  };
}
