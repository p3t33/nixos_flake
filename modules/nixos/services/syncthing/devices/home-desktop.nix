{ config, lib, ... }:
let
  deviceName = "home-desktop";
  cfg = config.customOptions.syncthing.enableRemoteDevice."${deviceName}";
in
{
  options.customOptions.syncthing = {
    enableRemoteDevice."${deviceName}" = lib.mkEnableOption "Enable Syncthing device: ${deviceName}";

    deviceNames."${deviceName}" = lib.mkOption {
      type = lib.types.str;
      default = deviceName;
      description = "Device name of ${deviceName}";
    };

  };

  config = lib.mkIf cfg {
    services.syncthing = {
      settings = {
        devices = {
          "${deviceName}" = {
            id = "TQ34X45-BKERB7F-LBQSEGR-ZAGQITL-RL5B242-PQSTHCX-2XBQBYL-ORMFTAH";
            autoAcceptFolders = true;
            name = deviceName;
            introducer = false;
          };
        };
      };
    };
  };
}
