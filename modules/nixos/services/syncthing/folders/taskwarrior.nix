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
          path = "${config.userDefinedGlobalVariables.syncthing.syncDir}/${taskwarrior}_data";
          devices = config.userDefinedGlobalVariables.syncthing.devicesToShareTaskWarriorFolderWith;
          versioning = config.userDefinedGlobalVariables.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
