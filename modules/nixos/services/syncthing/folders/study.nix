{ config, ... }:
let
  study = "study";
in
{
  services.syncthing = {
    settings = {
      folders = {
        "${study}" = {
          id = "${study}";
          path = "${config.customOptions.syncthing.syncDir}/${study}";
          devices = config.customOptions.syncthing.devicesToShareStudyFolderWith;
          versioning = config.customOptions.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
