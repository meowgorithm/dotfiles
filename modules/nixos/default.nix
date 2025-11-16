hostname: {
  pkgs,
  modulesPath,
  ...
}: let
  mainUser = "christian";
in {
  imports = [
    ./hardware-config-${hostname}.nix
    ./cachix.nix
  ];

  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
    trusted-users = ["root" mainUser];
  };

  nixpkgs.config = {
    allowUnfree = true;
    # cudaSupport = true;
  };

  boot = {
    isContainer = false;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    loader.timeout = 30;
  };

  hardware.graphics.enable = true;

  networking = {
    hostName = hostname;
    firewall.enable = false;
    networkmanager.enable = true;
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  hardware.keyboard.zsa.enable = true;
  hardware.opentabletdriver.enable = true;

  services.hardware.bolt.enable = true;

  time = {
    timeZone = "America/New_York";
    hardwareClockInLocalTime = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true;
  };

  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    QT_FONT_DPI = "192";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
  };

  # Hyprland
  programs.hyprland.enable = true;
  programs.hyprlock.enable = true;
  services.hypridle.enable = true;

  virtualisation.docker.enable = true;

  # Virt Manager
  programs.virt-manager.enable = true;
  users.groups.libvirtd.members = [mainUser];
  virtualisation.libvirtd.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;

  environment.systemPackages = with pkgs; [
    # blender
    # cudatoolkit

    cachix
    faac
    faad2
    git
    git-lfs
    gnupg
    lsof
    pinentry-curses
    sqlite-interactive

    gcc
    gmp
    ncurses
    xz
    pkg-config

    _1password-cli
    _1password-gui
    alejandra
    bash-language-server
    brotli
    cargo
    clang
    coreutils
    curl
    delve
    direnv
    duf
    dunst
    element
    elmPackages.elm
    elmPackages.elm-language-server
    ffmpeg
    firefox
    fzf
    getopt
    gh
    ghostty
    gnumake
    gnupg
    gnused
    gnutar
    go-task
    google-chrome
    haskellPackages.cabal-fmt
    helix
    htop
    hyprpaper
    imagemagick
    jq
    kitty
    librsvg
    libwebp
    lua-language-server
    luajit
    moreutils
    nil
    nodePackages_latest.prettier
    nodePackages_latest.svgo
    nodejs
    optipng
    postgresql
    python3
    redis
    rio
    ripgrep
    rust-analyzer
    rustc
    semgrep
    shellcheck
    shfmt
    stylua
    tmux
    tree
    tree-sitter
    ttyd
    vim
    vim-language-server
    vscode
    vscode-langservers-extracted
    waybar
    wget
    wofi
    yaml-language-server
    z-lua
    zlib
    zopfli

    go
    gofumpt
    golangci-lint
    golangci-lint-langserver
    gopls
    goreleaser
    gotools
  ];

  fonts.packages = with pkgs; [
    ibm-plex
    inter
    jetbrains-mono
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    source-han-sans
    ubuntu-classic
  ];

  security.sudo.wheelNeedsPassword = false;
  users.users.${mainUser} = {
    isNormalUser = true;
    extraGroups = ["wheel" "docker"];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILxWe2rXKoiO6W14LYPVfJKzRfJ1f3Jhzxrgjc/D4tU7"
    ];
    packages = with pkgs; [
      discord
      slack
      spotify
    ];
  };

  services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = false;
  };

  programs.ssh.startAgent = true;

  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "no";
    settings.PasswordAuthentication = true;
  };

  services.avahi = {
    enable = true;
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
    };
  };

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };

  system.stateVersion = "22.05";
}
