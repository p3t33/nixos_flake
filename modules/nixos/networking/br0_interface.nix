{ config, lib, ... }:
let
  cfg = config.custom.networking.bridgedInterface;
in
{
  options.custom.networking.bridgedInterface = {
    enable = lib.mkEnableOption "Enable bridged network configuration";
    name = lib.mkOption {
      type = lib.types.str;
      default = "br0";
      description = "The name of the bridge interface.";
    };

    memberInterfaces = lib.mkOption { # Renamed from physicalInterface
      type = lib.types.str;
      default = "enp0s13f0u3";
      description = "The physical network interface to be enslaved to the bridge.";
    };
  };

  config = lib.mkIf cfg.enable {
    networking = {
      firewall.trustedInterfaces = [ cfg.name ];

      # Define the bridge interface
      bridges.${cfg.name}.interfaces =
        [ cfg.memberInterfaces ];

      # Enable DHCP on the bridge
      interfaces.${cfg.name}.useDHCP = true;

      # Ensure the physical interface is free for the bridge to manage
      interfaces.${cfg.memberInterfaces} = {
        ipv4.addresses = [ ];
        useDHCP = false;
      };

      # As I am using both "static netowrk configurations" and dynamic once via networkmanager,
      # I had to disable networkmanager for those interface as it was conflicting with my original
      # intent.
      #
      # As a side note networkmanager can be used to define the bridge interface.
      networkmanager.unmanaged = [
        "interface-name:${cfg.name}"
        "interface-name:${cfg.memberInterfaces}"
      ];
    };
  };

  # sudo sysctl net.bridge.bridge-nf-call-ip6tables = 0
  # sudo sysctl net.bridge.bridge-nf-call-iptables = 0
  # sudo sysctl net.bridge.bridge-nf-call-arptables = 0
  #
  # sudo sysctl -w net.bridge.bridge-nf-call-ip6tables=0
  # sudo sysctl -w net.bridge.bridge-nf-call-arptables=0
  # sudo sysctl -w net.bridge.bridge-nf-call-iptables = 0
}

