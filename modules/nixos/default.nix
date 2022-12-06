hostname: {
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    ./hardware-config-${hostname}.nix
  ];

  boot = {
    isContainer = false;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  networking.hostName = hostname;
  networking.firewall.enable = false;

  time.timeZone = "America/New_York";
  time.hardwareClockInLocalTime = true;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true;
  };

  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
    trusted-users = ["root" "christian"];
  };

  nixpkgs.config = {
    allowUnfree = true;
    cudaSupport = true;
  };

  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    QT_FONT_DPI = "192";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
  };

  hardware = {
    opengl.enable = true;
    video.hidpi.enable = true;
  };

  services.hardware.bolt.enable = true;

  services.autorandr.enable = true;
  services.xserver = {
    enable = true;
    dpi = 192;
    layout = "us";
    xkbOptions = "eurosign:e, compose:menu, caps:escape";
    xrandrHeads = [{output = "DP-0";} {output = "DP-2";}];
    videoDrivers = ["nvidia"];

    wacom.enable = true;
    libinput.enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    displayManager = {
      defaultSession = "none+xmonad";
      lightdm = {
        greeters.enso = {
          enable = true;
          blur = true;
        };
      };

      sessionCommands = ''
        xrander --output DP-0 --mode2560x2880 --pos 0x0 --output DP-2 --mode 2560x2880 --pos 2560x0
      '';
    };
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  virtualisation.docker.enable = true;

  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    blender
    cachix
    git
    gnupg
    lguf-brightness
    lsof
    pinentry
    sqlite-interactive
    tmux
    vim
    wget
    xmonad-with-packages
    xtrlock-pam
  ];

  fonts.fonts = with pkgs; [
    jetbrains-mono
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-emoji
    noto-fonts-extra
    ubuntu_font_family
  ];

  users.users.christian = {
    isNormalUser = true;
    extraGroups = ["wheel" "docker"];
    packages = with pkgs; [
      slack
      discord
    ];
  };

  services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "curses";
    enableSSHSupport = false;
  };

  programs.ssh.startAgent = true;

  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
  };

  services.avahi = {
    enable = true;
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
    };
  };

  system.stateVersion = "22.05";
}
