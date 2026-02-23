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

  environment.systemPackages = with pkgs; [
    moolticute
    syncthing
    git-review # cli tool to interact with gerrit.
    vim
  ];
}
