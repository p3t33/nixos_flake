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
          path = "${config.customOptions.syncthing.syncDir}/${database}";
          devices = config.customOptions.syncthing.devicesToShareDatabaseFolderWith;
          versioning = config.customOptions.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
