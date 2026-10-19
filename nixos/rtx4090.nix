{config, ...}: {
  # The 4K panel is wired to the RTX 4090, but Mesa has no accelerated OpenGL
  # for Ada, so while nouveau owns the card Umbriel composites on the Ryzen
  # iGPU and runs its FX shaders through nvk/zink on the 4090. Every frame is
  # then handed across the two GPUs, which is what makes transitions crawl.
  # hardware.nvidia.* only tunes the driver; the driver itself is only
  # activated (and nouveau blacklisted) when "nvidia" appears here.
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = true;
    powerManagement.finegrained = false; # The dGPU drives the display, so no PRIME offload
    open = true; # Ada is supported by the open kernel module
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.production;
  };
}
