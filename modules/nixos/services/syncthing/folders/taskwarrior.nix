{ config, lib, ... }:
let
  cfg = config.customOptions.syncthing.enableFolder.taskwarrior;
  taskwarrior = "taskwarrior";
in
{
  options.customOptions.syncthing =
  {
    enableFolder.taskwarrior = lib.mkEnableOption "Enable Syncthing folder: taskwarrior";
    devicesToShareTaskWarriorFolderWith = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.str;
      description = "List of devices to use for folder synchronization.";
    };
  };

  config = lib.mkIf cfg {
    services.syncthing = {
      settings = {
        folders = {
          "${taskwarrior}" = {
            id = "${taskwarrior}";
            path = "${config.customOptions.syncthing.syncDir}/${taskwarrior}_data";
            devices = config.customOptions.syncthing.devicesToShareTaskWarriorFolderWith;
            versioning = config.customOptions.syncthing.simpleFileVersioningForBackUpMachinesOnly;
          };
        };
      };
    };
  };
}
