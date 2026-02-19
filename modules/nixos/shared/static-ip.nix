{ config, lib, hostSpecific, ...}:
{
  options.custom.shared = {
    ${hostSpecific.hostName} = {
      subnetPrefix = lib.mkOption {
        default = "192.168.1.";
        type = lib.types.str;
        description = "Defines the static IP subnet prefix used by machines on private network";
      };

      ip = lib.mkOption {
        default = "${config.custom.shared.${hostSpecific.hostName}.subnetPrefix}73";
        type = lib.types.str;
        description = "Defines the static IP used by the specific machine";
      };

      gateway = lib.mkOption {
        default = "${config.custom.shared.${hostSpecific.hostName}.subnetPrefix}1";
        type = lib.types.str;
        description = "Defines the gateway IP";
      };
    };
  };
}

