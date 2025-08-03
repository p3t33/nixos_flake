{ config, ... }:
{

  imports = [
    ../../modules/nixos/services # imported via default.nix
  ];

  services.syncthing.enable = true;
  programs.adb.enable = true;

  custom = {
    profiles.systemServices = {
      core.enable = true;
      desktop.enable = true;
    };

    connectivity.bluetooth.enable = true;

    services.syncthing = {
      settings = {
        devices = {
          homelab.enable = true;
          home-desktop.enable = true;
        };

        folders = {
          taskwarrior = {
            enable = true;
            devices = [
              "${config.services.syncthing.settings.devices.homelab.name}"
              "${config.services.syncthing.settings.devices.home-desktop.name}"
            ];
          };

          dev_resources = {
            enable = true;
            devices = [
              "${config.services.syncthing.settings.devices.homelab.name}"
              "${config.services.syncthing.settings.devices.home-desktop.name}"
            ];
          };
        };
      };
    };
  };
}
