{ config, ... }:
{
  services.syncthing = {
    settings = {
        folders = {
            "taskwarrior" = {
                id = "taskwarrior";
                path = "${config.userDefinedGlobalVariables.syncthingSyncDir}/taskwarrior_data";
                devices = config.userDefinedGlobalVariables.devicesToShareTaskWarriorFolderWith;
            };
        };
    };
  };
}
