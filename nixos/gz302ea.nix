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

  # Prevent suspend on lid close. The GZ302EA is a detachable tablet
  # and closing the lid should just turn off the internal display.
  services.logind.settings.Login = {
    HandleLidSwitch = "ignore";
    HandleLidSwitchExternalPower = "ignore";
    HandleLidSwitchDocked = "ignore";
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
