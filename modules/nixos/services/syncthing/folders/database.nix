{ config, lib, ... }:
let
  cfg = config.custom.services.syncthing;
  database = "database";
in
{
  options.custom.services.syncthing.foldersToShare.database = {
    enable = lib.mkEnableOption "Enable Syncthing folder: database";
    devicesToShareWith  = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.str;
      description = "List of devices to use for folder synchronization.";
    };
  };

  config = lib.mkIf cfg.foldersToShare.database.enable {
    services.syncthing = {
      settings = {
        folders = {
          "${database}" = {
            id = "${database}";
            path = "${cfg.syncDir}/${database}";
            devices = cfg.foldersToShare.database.devicesToShareWith;
            versioning = cfg.simpleFileVersioningForBackUpMachinesOnly;
          };
        };
      };
    };
  };
}
