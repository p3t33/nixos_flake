{ config, lib, ... }:
let
  deviceName = "work-pc";
  cfg = config.customOptions.syncthing.enableRemoteDevice.${deviceName};
in
{
  options.customOptions.syncthing = {
    enableRemoteDevice.${deviceName} = lib.mkEnableOption "Enable Syncthing device: work-pc";

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
            id = "Z47X7UP-AFRW6CM-UUWCAFF-A5BA6C4-PKEU7IR-XDHH6IE-N7JL54R-RTWLNAG";
            autoAcceptFolders = true;
            name = deviceName;
            introducer = false;
          };
        };
      };
    };
  };
}
