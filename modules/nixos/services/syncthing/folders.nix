{ config, lib, name, dirName, ... }:
let
  cfg = config.custom.services.syncthing;
  # name = "dev_resources";
  # dirName = "dev_resources";
in
{
  options.custom.services.syncthing.settings.folders.${name} = {
    enable = lib.mkEnableOption "Enable Syncthing folder: ${name}";
    devices  = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.str;
      description = "List of devices to use for folder synchronization.";
    };
  };

  config = lib.mkIf cfg.settings.folders.${name}.enable {
    services.syncthing = {
      settings = {
        folders = {
          "${name}" = {
            id = "${name}";
            path = "${cfg.syncDir}/${dirName}";
            devices = cfg.settings.folders.${name}.devices;
            versioning = cfg.simpleFileVersioningForBackUpMachinesOnly;
          };
        };
      };
    };
  };
}
