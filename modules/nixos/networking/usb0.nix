{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.usb0Static;
in
{
  options.customOptions.enableModule.usb0Static = lib.mkEnableOption "Enable a static IP profile for usb0 via NetworkManager";

  config = lib.mkIf cfg {
    networking.networkmanager.ensureProfiles.profiles.usb0-static = {

      connection = {
        id              = "usb0-static";
        type            = "ethernet";
        "interface-name" = "usb0";
        autoconnect     = true;
      };

      ipv4 = {
        address1 = "192.168.99.1/24";
        method   = "manual"; # static ip
      };

      ipv6.method = "ignore";
    };
  };
}

