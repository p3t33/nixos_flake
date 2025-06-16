{ config, ... }:
let
  documents = "documents";
in
{
  services.syncthing = {
    settings = {
      folders = {
        "${documents}" = {
          id = "${documents}";
          path = "${config.customOptions.syncthing.syncDir}/${documents}";
          devices = config.customOptions.syncthing.devicesToShareDocumentsFolderWith;
          versioning = config.customOptions.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
