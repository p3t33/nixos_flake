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
  };
}

