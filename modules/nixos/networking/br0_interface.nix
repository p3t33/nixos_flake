{ config, ... }:
{
  networking = {
    firewall.trustedInterfaces = [ config.userDefinedGlobalVariables.bridgedNetwork.bridgeName ];

    # Define the bridge interface
    bridges.${config.userDefinedGlobalVariables.bridgedNetwork.bridgeName}.interfaces =
      [ config.userDefinedGlobalVariables.bridgedNetwork.physicalInterface ];

    # Enable DHCP on the bridge
    interfaces.${config.userDefinedGlobalVariables.bridgedNetwork.bridgeName}.useDHCP = true;

    # Ensure the physical interface is free for the bridge to manage
    interfaces.${config.userDefinedGlobalVariables.bridgedNetwork.physicalInterface} = {
      ipv4.addresses = [ ];
      useDHCP = false;
    };

    # As I am using both "static netowrk configurations" and dynamic once via networkmanager,
    # I had to disable networkmanager for those interface as it was conflicting with my original
    # intent.
    #
    # As a side note networkmanager can be used to define the bridge interface.
    networkmanager.unmanaged = [
      "interface-name:${config.userDefinedGlobalVariables.bridgedNetwork.bridgeName}"
      "interface-name:${config.userDefinedGlobalVariables.bridgedNetwork.physicalInterface}"
    ];
  };
}

