{ config, ... }:
let
  devResources = "dev_resources";
in
{
  services.syncthing = {
    settings = {
      folders = {
        "${devResources}" = {
          id = "${devResources}";
          path = "${config.userDefinedGlobalVariables.syncthing.syncDir}/${devResources}";
          devices = config.userDefinedGlobalVariables.syncthing.devicesToShareDevResourcesFolderWith;
          versioning = config.userDefinedGlobalVariables.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
