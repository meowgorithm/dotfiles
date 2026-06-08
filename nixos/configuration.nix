# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  config,
  lib,
  pkgs,
  ...
}: let
  mainUser = "christian";
in {
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "baobao"; # Define your hostname.

  # Configure network connections interactively with nmcli or nmtui.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Windows expects the clock to be in local time. This setting allows Windows
  # and Linux to work on the same system.
  time.hardwareClockInLocalTime = true;

  # Enable dynamic linking.
  programs.nix-ld.enable = true;

  nixpkgs.config.allowUnfree = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # services.pulseaudio.enable = true;
  # OR
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;

  users.users.${mainUser} = {
    isNormalUser = true;
    extraGroups = ["wheel" "postgres"];
    packages = [];
  };

  security.sudo.extraRules = [
    {
      users = [mainUser];
      commands = [
        {
          command = "ALL";
          options = ["NOPASSWD"];
        }
      ];
    }
  ];

  programs.firefox.enable = true;

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  environment.systemPackages = with pkgs; [
    _1password-gui
    air
    alejandra
    bash
    brightnessctl
    clang
    curl
    delve
    direnv
    duf
    efibootmgr
    feh
    ffmpeg
    fuzzel
    fzf
    gcc
    ghostty
    git
    git-lfs
    gmp
    gnumake
    gnupg
    go
    gofumpt
    golangci-lint
    golangci-lint-langserver
    google-chrome
    gopls
    go-task
    gotools
    gum
    haskellPackages.cabal-fmt
    helix
    hivemind
    htop
    imagemagick
    jq
    kitty
    mako
    mise
    neovim
    nil
    nix-bash-completions
    nodejs
    obsidian
    optipng
    pkg-config
    prettier
    psmisc
    quickshell
    rio
    ripgrep
    rtk
    shellcheck
    shfmt
    slack
    stylua
    swaybg
    tailscale
    taplo
    tmux
    vim
    wget
    wl-clipboard
    xz
    yaml-language-server
    zellij
    zlib
    zoom-us
    z-lua
  ];

  fonts.packages = with pkgs; [
    jetbrains-mono
    maple-mono.NF-CN
    noto-fonts
  ];

  programs = {
    uwsm.enable = true;
    hyprland = {
      enable = true;
      withUWSM = true;
      xwayland.enable = true;
    };
    hyprlock.enable = true;
    bash.completion.enable = true;
  };

  services = {
    hypridle.enable = true;
    openssh.enable = true;
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
      publish = {
        enable = true;
        addresses = true;
        workstation = true;
      };
    };
    postgresql = {
      enable = true;
      ensureDatabases = ["christian"];
      ensureUsers = [
        {
          name = "christian";
          ensureDBOwnership = true;
          ensureClauses = {
            superuser = true;
          };
        }
      ];
      authentication = lib.mkOverride 10 ''
        local all all trust
        host  all all 127.0.0.1/32 trust
        host  all all ::1/128 trust
      '';
    };
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "26.05"; # Did you read the comment?
}
