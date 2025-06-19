{ config, lib, ... }:
let
  cfg = config.customOptions.syncthing.enableFolder.database;
  database = "database";
in
{
  options.customOptions.syncthing = {
    enableFolder.database = lib.mkEnableOption "Enable Syncthing folder: database";
    devicesToShareDatabaseFolderWith = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.str;
      description = "List of devices to use for folder synchronization.";
    };
  };

  config = lib.mkIf cfg {
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
  };
}
