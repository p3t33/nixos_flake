{ config, lib, ... }:
let
  deviceName = "kvm-nixos-server";
  cfg = config.customOptions.syncthing.enableRemoteDevice."${deviceName}";
in
{
  options.customOptions.syncthing = {
    enableRemoteDevice."${deviceName}" = lib.mkEnableOption "Enable Syncthing device: kvm-nixos-server";
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
            id = "VMTWSEY-4TLKW4M-5KWF4NP-R44ZCUB-532E53K-WGO2YX3-GCRDHBV-3WGSHAI";
            autoAcceptFolders = true;
            name = deviceName;
            introducer = false;
          };
        };
      };
    };
  };
}
