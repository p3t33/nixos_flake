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
          path = "${config.userDefinedGlobalVariables.syncthing.syncDir}/${study}";
          devices = config.userDefinedGlobalVariables.syncthing.devicesToShareStudyFolderWith;
          versioning = config.userDefinedGlobalVariables.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
