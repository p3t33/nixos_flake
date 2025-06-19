{ config, lib, ... }:
let
  cfg = config.custom.services.syncthing;
  devResources = "dev_resources";
in
{
  options.custom.services.syncthing.foldersToShare.devResources = {
    enable = lib.mkEnableOption "Enable Syncthing folder: dev_resources";
    devicesToShareWith  = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.str;
      description = "List of devices to use for folder synchronization.";
    };
  };

  config = lib.mkIf cfg.foldersToShare.devResources.enable {
    services.syncthing = {
      settings = {
        folders = {
          "${devResources}" = {
            id = "${devResources}";
            path = "${cfg.syncDir}/${devResources}";
            devices = cfg.foldersToShare.devResources.devicesToShareWith;
            versioning = cfg.simpleFileVersioningForBackUpMachinesOnly;
          };
        };
      };
    };
  };
}
