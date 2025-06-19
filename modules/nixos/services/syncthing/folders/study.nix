{ config, lib, ... }:
let
  cfg = config.customOptions.syncthing.enableFolder.study;
  study = "study";
in
{
  options.customOptions.syncthing = {
    enableFolder.study = lib.mkEnableOption "Enable Syncthing folder: study";
    devicesToShareStudyFolderWith = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.str;
      description = "List of devices to use for folder synchronization.";
    };
  };

  config = lib.mkIf cfg {
    services.syncthing = {
      settings = {
        folders = {
          "${study}" = {
            id = "${study}";
            path = "${config.customOptions.syncthing.syncDir}/${study}";
            devices = config.customOptions.syncthing.devicesToShareStudyFolderWith;
            versioning = config.customOptions.syncthing.simpleFileVersioningForBackUpMachinesOnly;
          };
        };
      };
    };
  };
}
