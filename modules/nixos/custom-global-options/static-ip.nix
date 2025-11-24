{ config, lib, hostSpecific, ...}:
{
  options.customGlobal = {
    ${hostSpecific.hostName} = {
      subnetPrefix = lib.mkOption {
        default = "192.168.1.";
        type = lib.types.str;
        description = "Defines the static IP subnet prefix used by homelab machines";
      };

      ip = lib.mkOption {
        default = "${config.customGlobal.${hostSpecific.hostName}.subnetPrefix}73";
        type = lib.types.str;
        description = "Defines the static IP used by the homelab machine";
      };

      gateway = lib.mkOption {
        default = "${config.customGlobal.${hostSpecific.hostName}.subnetPrefix}1";
        type = lib.types.str;
        description = "Defines the gateway IP for the homelab machine";
      };
    };
  };
}

