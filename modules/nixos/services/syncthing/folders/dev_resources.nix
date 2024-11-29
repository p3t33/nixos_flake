{ config, ... }:
{
  services.syncthing = {
    settings = {
        folders = {
            "dev_resources" = {
                id = "dev_resources";
                path = "${config.userDefinedGlobalVariables.syncthingSyncDir}/dev_resources";
                devices = config.userDefinedGlobalVariables.devicesToShareDevResourcesFolderWith;
            };
        };
    };
  };
}
