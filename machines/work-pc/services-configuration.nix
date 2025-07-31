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

      remoteDevices = {
        homelab.enable = true;
        home-desktop.enable = true;
      };

      foldersToShare = {
        taskwarrior = {
          enable = true;
          devicesToShareWith = [
            "${config.custom.services.syncthing.remoteDevices.homelab.name}"
            "${config.custom.services.syncthing.remoteDevices.home-desktop.name}"
          ];
        };

        devResources = {
          enable = true;
          devicesToShareWith = [
            "${config.custom.services.syncthing.remoteDevices.homelab.name}"
            "${config.custom.services.syncthing.remoteDevices.home-desktop.name}"
          ];
        };
      };

    };
  };
}
