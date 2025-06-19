{ config, lib, ... }:
let
  cfg = config.customOptions.syncthing.enableFolder.documents;
  documents = "documents";
in
{
  options.customOptions.syncthing = {
    enableFolder.documents = lib.mkEnableOption "Enable Syncthing folder: documents";
    devicesToShareDocumentsFolderWith = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.str;
      description = "List of devices to use for folder synchronization.";
    };
  };

  config = lib.mkIf cfg {
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
  };
}
