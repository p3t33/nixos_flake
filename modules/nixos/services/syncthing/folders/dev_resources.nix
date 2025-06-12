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
          path = "${config.hostSpecification.syncthing.syncDir}/${devResources}";
          devices = config.hostSpecification.syncthing.devicesToShareDevResourcesFolderWith;
          versioning = config.hostSpecification.syncthing.simpleFileVersioningForBackUpMachinesOnly;
        };
      };
    };
  };
}
