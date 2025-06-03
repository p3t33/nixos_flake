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
          path = "${config.userDefinedGlobalVariables.syncthing.syncDir}/${documents}";
          devices = config.userDefinedGlobalVariables.syncthing.devicesToShareDocumentsFolderWith;
          versioning = config.userDefinedGlobalVariables.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
