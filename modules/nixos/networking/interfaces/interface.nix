{ config, lib, nameToSetWithUdev, macAddress, ... }:

let
  cfg = config.custom.networking.interfaces.${nameToSetWithUdev};
in
{
  options.custom.networking.interfaces.${nameToSetWithUdev} = {
    enable = lib.mkEnableOption "Enable ${nameToSetWithUdev} network interface with udev rule for device renaming";

    macAddressToApplyUdevRuleOn = lib.mkOption {
      type = lib.types.str;
      default = macAddress;
      example = "aa:bb:cc:dd:ee:ff";
      description = "MAC address of the network device to match in udev rule";
    };

    staticIp = {
      enable = lib.mkEnableOption "Enable a static IP profile for ${nameToSetWithUdev} via NetworkManager";

      address = lib.mkOption {
        type = lib.types.str;
        default = "192.168.1.1/24";
        example = "10.0.0.1/24";
        description = "Static IP address with CIDR notation (e.g., 192.168.1.1/24)";
      };

      profileId = lib.mkOption {
        type = lib.types.str;
        default = "${nameToSetWithUdev}-static";
        example = "my-ethernet-static";
        description = "NetworkManager connection profile name (what shows in 'nmcli connection show')";
      };
    };
  };

  config = lib.mkMerge [
    # Udev rule configuration (when main enable is true)
    (lib.mkIf cfg.enable {
      services.udev.extraRules = ''
        SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="${cfg.macAddressToApplyUdevRuleOn}", NAME="${nameToSetWithUdev}"
      '';
    })

    # Static IP configuration (requires both enable and staticIp.enable)
    (lib.mkIf (cfg.enable && cfg.staticIp.enable) {
      networking.networkmanager.ensureProfiles.profiles.${cfg.staticIp.profileId} = {

        connection = {
          id              = cfg.staticIp.profileId;
          type            = "ethernet";
          "interface-name" = nameToSetWithUdev;
          autoconnect     = true;
        };

        ipv4 = {
          address1 = cfg.staticIp.address;
          method   = "manual"; # static ip
        };

        ipv6.method = "ignore";
      };
    })
  ];
}
