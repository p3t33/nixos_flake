{ lib, ...}:
{
  options.customGlobal = {
    localHostIPv4 = lib.mkOption {
      default = "127.0.0.1";
      type = lib.types.str;
      description = "Defines the static IP used by the homelab machine";
    };

    # global
    anyIPv4 = lib.mkOption {
        default = "0.0.0.0";
        type = lib.types.str;
        description = "Defines an IPv4 address that binds to all available network interfaces.";
    };
  };
}
