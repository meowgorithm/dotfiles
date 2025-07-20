{...}: {
  programs.ssh = {
    enable = true;
    includes = ["~/.ssh/config.local"];
    matchBlocks = {
      "*" = {
        identityFile = "~/.ssh/id_ed25519";
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
