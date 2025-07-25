hostname: {
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    ./hardware-config-${hostname}.nix
    ./cachix.nix
  ];

  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
    trusted-users = ["root" "christian"];
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

  programs.hyprland.enable = true;
  programs.hyprlock.enable = true;
  services.hypridle.enable = true;

  virtualisation.docker.enable = true;

  environment.systemPackages = with pkgs; [
    # blender
    cachix
    # cudatoolkit
    faac
    faad2
    git
    gnupg
    lguf-brightness
    lsof
    pinentry
    sqlite-interactive
    tmux
    vim
    wget
  ];

  fonts.packages = with pkgs; [
    jetbrains-mono
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-emoji
    noto-fonts-extra
    source-han-sans
    ubuntu_font_family
  ];

  security.sudo.wheelNeedsPassword = false;
  users.users.christian = {
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
