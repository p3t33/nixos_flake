{ config, lib, ... }:
let
  cfg = config.custom.services.syncthing;
  study = "study";
in
{
  options.custom.services.syncthing.foldersToShare.study = {
    enable = lib.mkEnableOption "Enable Syncthing folder: study";
    devicesToShareWith  = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.str;
      description = "List of devices to use for folder synchronization.";
    };
  };

  config = lib.mkIf cfg.foldersToShare.study.enable {
    services.syncthing = {
      settings = {
        folders = {
          "${study}" = {
            id = "${study}";
            path = "${cfg.syncDir}/${study}";
            devices = cfg.foldersToShare.documents.devicesToShareWith;
            versioning = cfg.simpleFileVersioningForBackUpMachinesOnly;
          };
        };
      };
    };
  };
}
