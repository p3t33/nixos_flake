{ config, lib, ... }:
let
  deviceName = "kvm-nixos-server";
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
            id = "VMTWSEY-4TLKW4M-5KWF4NP-R44ZCUB-532E53K-WGO2YX3-GCRDHBV-3WGSHAI";
            autoAcceptFolders = true;
            name = cfg.name;
            introducer = false;
          };
        };
      };
    };
  };
}
