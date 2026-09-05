{ lib, ... }:
{
  options.custom.shared.lan = {
    ipv4Cidr = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = "192.168.1.0/24";
      description = "Private LAN IPv4 network used by network-scoped service policies.";
    };

    hosts = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule {
        options.ipv4Address = lib.mkOption {
          type = lib.types.str;
          description = "LAN IPv4 address reserved for this host through OPNsense DHCP.";
        };
      });
      readOnly = true;
      default = {
        nas.ipv4Address = "192.168.1.20";
        "home-assistant".ipv4Address = "192.168.1.21";
        helper.ipv4Address = "192.168.1.22";
      };
      description = "Inventory of router-reserved LAN host addresses; does not configure NixOS interfaces.";
    };
  };
}
