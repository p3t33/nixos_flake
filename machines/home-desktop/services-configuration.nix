{ config, ... }:
{

  imports = [
    ../../modules/nixos/services # imported via default.nix
  ];

  customOptions = {
    syncthing = {
      devicesToShareTaskWarriorFolderWith = [
        "${config.customOptions.syncthing.deviceNames.homelab}"
        "${config.customOptions.syncthing.deviceNames.work-pc}"
      ];
      devicesToShareDevResourcesFolderWith = [
        "${config.customOptions.syncthing.deviceNames.homelab}"
        "${config.customOptions.syncthing.deviceNames.work-pc}"
      ];
      devicesToShareDatabaseFolderWith = [
        "${config.customOptions.syncthing.deviceNames.homelab}"
      ];
      devicesToShareDocumentsFolderWith = [
        "${config.customOptions.syncthing.deviceNames.homelab}"
      ];
      devicesToShareStudyFolderWith = [
        "${config.customOptions.syncthing.deviceNames.homelab}"
      ];

      enableRemoteDevice = {
        homelab = true;
        work-pc = true;
      };

      enableFolder = {
        taskwarrior = true;
        devResources = true;
        database = true;
        documents = true;
        study = true;
      };
    };

    enableServicesProfile = {
      core = true;
      desktop = true;
    };

    enableModule = {
      syncthing = true;
      trezor = true;
    };
  };
}
