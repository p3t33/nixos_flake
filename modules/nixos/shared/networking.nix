{ lib, ...}:
{
  options.custom.shared = {
    localHostIPv4 = lib.mkOption {
      default = "127.0.0.1";
      type = lib.types.str;
      description = "Defines the local host ip";
    };

    appsDomain = lib.mkOption {
      type = lib.types.str;
      default = "apps.home.medrish.com";
      description = "Base domain for internally hosted applications.";
    };

    # global
    anyIPv4 = lib.mkOption {
        default = "0.0.0.0";
        type = lib.types.str;
        description = "Defines an IPv4 address that binds to all available network interfaces.";
    };
  };
}
