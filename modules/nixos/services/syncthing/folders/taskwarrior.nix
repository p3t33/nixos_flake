{ config, lib, ... }:
let
  cfg = config.custom.services.syncthing;
  taskwarrior = "taskwarrior";
in
{
  options.custom.services.syncthing.foldersToShare.taskwarrior = {
    enable = lib.mkEnableOption "Enable Syncthing folder: taskwarrior";
    devicesToShareWith = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.str;
      description = "List of devices to use for folder synchronization.";
    };
  };

  config = lib.mkIf cfg.foldersToShare.taskwarrior.enable {
    services.syncthing = {
      settings = {
        folders = {
          "${taskwarrior}" = {
            id = "${taskwarrior}";
            path = "${cfg.syncDir}/${taskwarrior}_data";
            devices = cfg.foldersToShare.taskwarrior.devicesToShareWith;
            versioning = cfg.simpleFileVersioningForBackUpMachinesOnly;
          };
        };
      };
    };
  };
}
