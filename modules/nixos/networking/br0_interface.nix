{ config, lib, ... }:
{
  options.customOptions.bridgedNetwork = lib.mkOption {
    default = {
      bridgeName = "br0";
      physicalInterface = "enp0s13f0u3";
    };
    type = lib.types.attrsOf lib.types.str;
    description = "Defines the color palette for the user interface";
  };

  config = {
    networking = {
      firewall.trustedInterfaces = [ config.customOptions.bridgedNetwork.bridgeName ];

      # Define the bridge interface
      bridges.${config.customOptions.bridgedNetwork.bridgeName}.interfaces =
        [ config.customOptions.bridgedNetwork.physicalInterface ];

      # Enable DHCP on the bridge
      interfaces.${config.customOptions.bridgedNetwork.bridgeName}.useDHCP = true;

      # Ensure the physical interface is free for the bridge to manage
      interfaces.${config.customOptions.bridgedNetwork.physicalInterface} = {
        ipv4.addresses = [ ];
        useDHCP = false;
      };

      # As I am using both "static netowrk configurations" and dynamic once via networkmanager,
      # I had to disable networkmanager for those interface as it was conflicting with my original
      # intent.
      #
      # As a side note networkmanager can be used to define the bridge interface.
      networkmanager.unmanaged = [
        "interface-name:${config.customOptions.bridgedNetwork.bridgeName}"
        "interface-name:${config.customOptions.bridgedNetwork.physicalInterface}"
      ];
    };
  };
}

