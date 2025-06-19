{ config, lib, ... }:
let
  cfg = config.custom.services.syncthing;
  documents = "documents";
in
{
  options.custom.services.syncthing.foldersToShare.documents = {
    enable = lib.mkEnableOption "Enable Syncthing folder: documents";
    devicesToShareWith = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.str;
      description = "List of devices to use for folder synchronization.";
    };
  };

  config = lib.mkIf cfg.foldersToShare.documents.enable {
    services.syncthing = {
      settings = {
        folders = {
          "${documents}" = {
            id = "${documents}";
            path = "${cfg.syncDir}/${documents}";
            devices = cfg.foldersToShare.documents.devicesToShareWith;
            versioning = cfg.simpleFileVersioningForBackUpMachinesOnly;
          };
        };
      };
    };
  };
}
