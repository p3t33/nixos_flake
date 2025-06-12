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
          path = "${config.hostSpecification.syncthing.syncDir}/${documents}";
          devices = config.hostSpecification.syncthing.devicesToShareDocumentsFolderWith;
          versioning = config.hostSpecification.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
