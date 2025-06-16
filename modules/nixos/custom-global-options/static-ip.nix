{ config, lib, hostSpecific, ...}:
{
  options.customGlobalOptions = {
    ${hostSpecific.hostName} = {
      subnetPrefix = lib.mkOption {
        default = "10.100.102.";
        type = lib.types.str;
        description = "Defines the static IP subnet prefix used by homelab machines";
      };

      ip = lib.mkOption {
        default = "${config.customGlobalOptions.${hostSpecific.hostName}.subnetPrefix}73";
        type = lib.types.str;
        description = "Defines the static IP used by the homelab machine";
      };

      gateway = lib.mkOption {
        default = "${config.customGlobalOptions.${hostSpecific.hostName}.subnetPrefix}1";
        type = lib.types.str;
        description = "Defines the gateway IP for the homelab machine";
      };
    };
  };
}

