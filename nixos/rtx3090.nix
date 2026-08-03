{config, ...}: {
  # hardware.nvidia.* only tunes the driver; the driver itself is only
  # activated (and nouveau blacklisted) when "nvidia" appears here.
  # This is the supported way to use the NVIDIA driver on Wayland.
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = true;
    powerManagement.finegrained = false; # PRIME offload only; off for a desktop 3090
    open = true; # Ampere is well-supported; A/B against false
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.production;
  };
}
