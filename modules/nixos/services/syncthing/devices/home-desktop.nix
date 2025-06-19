{ config, lib, ... }:
let
  deviceName = "home-desktop";
  cfg = config.custom.services.syncthing.remoteDevices.${deviceName};
in
{
  options.custom.services.syncthing.remoteDevices.${deviceName} = {
    enable = lib.mkEnableOption "Enable Syncthing device: homelab";
    name = lib.mkOption {
      type = lib.types.str;
      default = deviceName;
      description = "Device name of ${deviceName}";
    };
  };

  config = lib.mkIf cfg.enable {
    services.syncthing = {
      settings = {
        devices = {
          "${cfg.name}" = {
            id = "TQ34X45-BKERB7F-LBQSEGR-ZAGQITL-RL5B242-PQSTHCX-2XBQBYL-ORMFTAH";
            autoAcceptFolders = true;
            name = cfg.name;
            introducer = false;
          };
        };
      };
    };
  };
}
