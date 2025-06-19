{ config, lib, ... }:
let
  deviceName = "work-pc";
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
            id = "Z47X7UP-AFRW6CM-UUWCAFF-A5BA6C4-PKEU7IR-XDHH6IE-N7JL54R-RTWLNAG";
            autoAcceptFolders = true;
            name = cfg.name;
            introducer = false;
          };
        };
      };
    };
  };
}
