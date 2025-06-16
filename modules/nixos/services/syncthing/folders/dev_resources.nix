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
          path = "${config.customOptions.syncthing.syncDir}/${devResources}";
          devices = config.customOptions.syncthing.devicesToShareDevResourcesFolderWith;
          versioning = config.customOptions.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
