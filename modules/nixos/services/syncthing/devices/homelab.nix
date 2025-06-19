{ config, lib, ... }:
let
  deviceName = "homelab";
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
            id = "XPCO572-XPKAN7M-BXTAVRT-2WZGOBR-QWMG6F7-3EHJ276-GUZJ2UW-ZVLRCQK";
            autoAcceptFolders = true;
            name = cfg.name;
            introducer = false;
          };
        };
      };
    };
  };
}
