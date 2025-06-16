{ config, ... }:
let
  taskwarrior = "taskwarrior";
in
{
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
}
