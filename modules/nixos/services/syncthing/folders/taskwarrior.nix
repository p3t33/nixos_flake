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
          path = "${config.hostSpecification.syncthing.syncDir}/${taskwarrior}_data";
          devices = config.hostSpecification.syncthing.devicesToShareTaskWarriorFolderWith;
          versioning = config.hostSpecification.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
