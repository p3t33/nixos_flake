{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./services-configuration.nix
    ./sops-configuration.nix
    ./disko-configuration.nix
    ../../modules/nixos # imported via default.nix
  ];

  custom = {
    profiles.system = {
      core.enable = true;
      desktop.enable = true;
      securityKeys.enable = true;
      virtualization.enable = true;
    };

    apps.wireshark.enable = true;

    networking = {
      bridgedInterface.enable = true;
      interfaces = {
        usbeth0.enable = true;
        alpha-sniffer.enable = true;
        svx = {
          enable = true;
          staticIp = {
            enable = true;
            address = "192.168.99.1/24";
          };
        };
      };
    };

    systemStateVersion = "24.05";
  };

  hardware.i2c.enable = true;

  # HP ZBook Firefly 14 G11: the mei_me driver blocks the ACPI S5 (power-off)
  # transition. During shutdown the driver tries to gracefully disconnect from
  # the Intel Management Engine firmware, but the handshake hangs — the kernel
  # waits for the callback to finish and never reaches power-off.
  #
  # The ME hardware runs independently on its own embedded processor and does
  # not need a Linux driver to function. Blacklisting mei_me simply prevents
  # the driver from loading, so there is no stuck shutdown callback to block
  # the power-off path. The only features lost are AMT remote management and
  # OS-level ME firmware updates (still possible via BIOS/UEFI).
  boot.blacklistedKernelModules = [ "mei_me" ];

  # HP ZBook Firefly 14 G11: the default ACPI reboot method hangs — black
  # screen with power LED on. reboot=efi also hangs. Trying PCI bus reset.
  boot.kernelParams = [ "reboot=pci" ];

  environment.systemPackages = with pkgs; [
    moolticute
    syncthing
    git-review # cli tool to interact with gerrit.
    vim
  ];
}
