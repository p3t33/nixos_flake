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
          path = "${config.hostSpecification.syncthing.syncDir}/${database}";
          devices = config.hostSpecification.syncthing.devicesToShareDatabaseFolderWith;
          versioning = config.hostSpecification.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
