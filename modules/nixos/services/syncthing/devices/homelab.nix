{ config, lib, ... }:
let
  deviceName = "homelab";
  cfg = config.customOptions.syncthing.enableRemoteDevice.${deviceName};
in
{
  options.customOptions.syncthing = {
    enableRemoteDevice."${deviceName}" = lib.mkEnableOption "Enable Syncthing device: homelab";

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
            id = "XPCO572-XPKAN7M-BXTAVRT-2WZGOBR-QWMG6F7-3EHJ276-GUZJ2UW-ZVLRCQK";
            autoAcceptFolders = true;
            name = deviceName;
            introducer = false;
          };
        };
      };
    };
  };
}
