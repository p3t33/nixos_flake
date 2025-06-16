{ pkgs, config, lib, ... }:
{

  options.customOptions.wireguard = lib.mkOption {
    type = lib.types.attrsOf (lib.types.oneOf [lib.types.str lib.types.int]);
    default = let
      base = "10.100.0";
    in {
      networkName = "wg0";
      externalInterface = "eno1";
      baseSubnet = base;
      gateway = "${base}.1";
      network = "${base}.0/24";
      listenPort = 51820;
    };
    description = "WireGuard base network settings";
  };

  config = {
    networking.firewall = {
      allowedUDPPorts = [ config.customOptions.wireguard.listenPort ];
    };

    sops.secrets.wireguard_key = {};
    networking.wireguard.interfaces = {

      ${config.customOptions.wireguard.networkName} = {
        # Determines the IP address and subnet of the server's end of the tunnel interface.
        ips = [ "${config.customOptions.wireguard.gateway}/24" ];

        listenPort = config.customOptions.wireguard.listenPort;
        # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
        # For this to work you have to set the dnsserver IP of your router (or dnsserver of choice) in your clients
        postSetup = ''
          ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s ${config.customOptions.wireguard.network} -o ${config.customOptions.wireguard.externalInterface} -j MASQUERADE
        '';

        # This undoes the above command
        postShutdown = ''
          ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s ${config.customOptions.wireguard.network} -o ${config.customOptions.wireguard.externalInterface} -j MASQUERADE
        '';

        privateKeyFile = config.sops.secrets.wireguard_key.path;

        peers = [
          {
            publicKey = "WiXR/lQSrTGf7fRkNGFqDGpr0f4h95ICwmvGu+NJQBA=";
            allowedIPs = [ "${config.customOptions.wireguard.baseSubnet}.3/32" ];
          }
          {
            publicKey = "+r85S5G10c3Ltc9NSMDzp3QOy0hT1pLm9Vb0cwmhxAE=";
            allowedIPs = [ "${config.customOptions.wireguard.baseSubnet}.5/32" ];
          }
        ];
      };
    };
  };
}

