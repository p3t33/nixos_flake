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
          path = "${config.hostSpecification.syncthing.syncDir}/${study}";
          devices = config.hostSpecification.syncthing.devicesToShareStudyFolderWith;
          versioning = config.hostSpecification.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
