{ config, lib, ... }:
let
  cfg = config.customOptions.syncthing.enableFolder.devResources;
  devResources = "dev_resources";
in
{
  options.customOptions.syncthing = {
    enableFolder.devResources = lib.mkEnableOption "Enable Syncthing folder: dev_resources";
    devicesToShareDevResourcesFolderWith = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.str;
      description = "List of devices to use for folder synchronization.";
    };
  };

  config = lib.mkIf cfg {
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
  };
}
