{ config, ... }:
let
  database = "database";
in
{
  services.syncthing = {
    settings = {
      folders = {
        "${database}" = {
          id = "${database}";
          path = "${config.userDefinedGlobalVariables.syncthing.syncDir}/${database}";
          devices = config.userDefinedGlobalVariables.syncthing.devicesToShareDatabaseFolderWith;
          versioning = config.userDefinedGlobalVariables.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
