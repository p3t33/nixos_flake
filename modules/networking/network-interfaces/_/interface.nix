{ config, lib, nameToSetWithUdev, macAddress ? null, usbVendorId ? null, usbProductId ? null, usbSerial ? null, ... }:

let
  cfg = config.custom.networking.interfaces.${nameToSetWithUdev};

  # Build udev match criteria based on available identifiers.
  # USB attributes (ATTRS) traverse parent devices to find vendor/product IDs,
  # which is needed for USB devices whose MAC address changes on each connection.
  # MAC matching (ATTR) matches on the network device itself.
  #
  # to get serial, idVendor, idProduct you can use
  #
  # lsusb -u(if it is connected via usb) or if you know the udev path
  #
  # udevadm info --query=all --name=/dev/<...>
  udevMatchCriteria =
    if cfg.usbVendorId != null && cfg.usbProductId != null then
      ''ATTRS{idVendor}=="${cfg.usbVendorId}", ATTRS{idProduct}=="${cfg.usbProductId}"''
      + lib.optionalString (cfg.usbSerial != null) '', ATTRS{serial}=="${cfg.usbSerial}"''
    else
      ''ATTR{address}=="${cfg.macAddressToApplyUdevRuleOn}"'';
in
{
  options.custom.networking.interfaces.${nameToSetWithUdev} = {
    enable = lib.mkEnableOption "Enable ${nameToSetWithUdev} network interface with udev rule for device renaming";

    macAddressToApplyUdevRuleOn = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = macAddress;
      example = "aa:bb:cc:dd:ee:ff";
      description = "MAC address of the network device to match in udev rule";
    };

    usbVendorId = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = usbVendorId;
      example = "0cad";
      description = "USB vendor ID to match in udev rule (use instead of MAC for devices with changing MAC addresses)";
    };

    usbProductId = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = usbProductId;
      example = "150d";
      description = "USB product ID to match in udev rule (used together with usbVendorId)";
    };

    usbSerial = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = usbSerial;
      example = "9903EBJ0021";
      description = "USB serial number to match in udev rule (optional, for distinguishing multiple identical devices)";
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
        SUBSYSTEM=="net", ACTION=="add", ${udevMatchCriteria}, NAME="${nameToSetWithUdev}"
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
