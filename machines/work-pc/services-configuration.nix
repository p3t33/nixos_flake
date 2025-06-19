{ config, ... }:
{

  imports = [
    ../../modules/nixos/services # imported via default.nix
  ];

  customOptions = {
    syncthing = {
      devicesToShareTaskWarriorFolderWith = [
        "${config.customOptions.syncthing.deviceNames.homelab}"
        "${config.customOptions.syncthing.deviceNames.home-desktop}"
      ];

      devicesToShareDevResourcesFolderWith = [
        "${config.customOptions.syncthing.deviceNames.homelab}"
        "${config.customOptions.syncthing.deviceNames.home-desktop}"
      ];

      enableRemoteDevice = {
        homelab = true;
        home-desktop = true;
      };

      enableFolder = {
        taskwarrior = true;
        devResources = true;
      };
    };

    enableServicesProfile = {
      core = true;
      desktop = true;
    };

    enableModule = {
      syncthing = true;
      bluetooth = true;
      adb = true;
    };
  };
}
