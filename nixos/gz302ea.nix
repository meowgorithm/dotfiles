{...}: {
  services.asusd.enable = true;

  # Freeze workaround.
  boot.kernelParams = ["amdgpu.cwsr_enable=0"];

  # Suspend-drain fix.
  boot.extraModprobeConfig = ''
    options mt7925e disable_aspm=1
  '';

  # Mark the ASUS ROG Flow Z13 (GZ302EA) detachable touchpad as internal
  # so libinput enables disable-while-typing. The touchpad connects over
  # USB and would otherwise be treated as external.
  services.udev.extraHwdb = ''
    touchpad:usb:v0b05p1a30:*
     ID_INPUT_TOUCHPAD_INTEGRATION=internal
  '';

  # Suspend on lid close, but only when no external monitor is connected.
  # Logind's HandleLidSwitch doesn't work reliably with UWSM-managed Wayland
  # sessions. A systemd service runs as the user so hyprctl is available.
  # When an external monitor is present, Hyprland disables the internal
  # display instead (see hyprland.lua).
  services.logind.settings.Login = {
    HandleLidSwitch = "ignore";
    HandleLidSwitchExternalPower = "ignore";
    HandleLidSwitchDocked = "ignore";
  };

  systemd.services.lid-suspend = {
    description = "Suspend on lid close if no external monitor";
    wantedBy = ["acpid.service"];
    after = ["acpid.service"];
    serviceConfig.Type = "oneshot";
    script = ''
      export PATH=/run/current-system/sw/bin
      SIG=$(ls /run/user/1000/hypr/ 2>/dev/null | head -1)
      if [ -n "$SIG" ]; then
        MONITORS=$(sudo -u christian XDG_RUNTIME_DIR=/run/user/1000 HYPRLAND_INSTANCE_SIGNATURE="$SIG" hyprctl -j monitors 2>/dev/null)
        HAS_EXT=$(echo "$MONITORS" | jq 'map(select(.name != "eDP-1")) | length' 2>/dev/null)
        if [ "$HAS_EXT" -gt 0 ] 2>/dev/null; then
          sudo -u christian XDG_RUNTIME_DIR=/run/user/1000 HYPRLAND_INSTANCE_SIGNATURE="$SIG" hyprctl eval 'hl.monitor({ output = "eDP-1", disabled = true })'
          exit 0
        fi
      fi
      systemctl suspend
    '';
  };

  services.acpid = {
    enable = true;
    handlers.lid-close = {
      event = "button/lid.*close";
      action = "systemctl start lid-suspend.service";
    };
  };

  # Prevent USB autosuspend for ASUS 5M webcam (636e:0bda).
  # The device fails to resume from suspend (error -22) and disconnects.
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="636e", ATTR{idProduct}=="0bda", ATTR{power/autosuspend}="-1"
    ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="636e", ATTR{idProduct}=="0bda", ATTR{power/control}="on"
  '';

  # Rebind the xHCI controller after suspend so the webcam re-enumerates.
  systemd.services.fix-webcam-resume = {
    description = "Fix ASUS webcam after suspend/resume";
    after = ["suspend.target" "hibernate.target" "hybrid-sleep.target"];
    wantedBy = ["suspend.target" "hibernate.target" "hybrid-sleep.target"];
    serviceConfig.Type = "oneshot";
    script = ''
      CONTROLLER="/sys/bus/pci/drivers/xhci_hcd/0000:c4:00.4"
      if [ -e "$CONTROLLER" ]; then
        echo "0000:c4:00.4" > /sys/bus/pci/drivers/xhci_hcd/unbind
        sleep 1
        echo "0000:c4:00.4" > /sys/bus/pci/drivers/xhci_hcd/bind
        sleep 2
      fi
    '';
  };
}
