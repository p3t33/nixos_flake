{ pkgs, config, lib, ... }:
let
  cfg = config.custom.vpn.wireguardServer;
in
{

  # external interface needs to match actual interface that is used for outside access
  # When moving the server form one machine to another(or just updating ip) it is
  # important to update port forwording of the router as well.
  #
  # To test connection(beside using systemd tools), use;
  # sudo wg show
  options.custom.vpn.wireguardServer =
  {
    enable = lib.mkEnableOption "Enable WireGuard server configuration";

    networkName = lib.mkOption {
      type = lib.types.str;
      default = "wg0";
      description = "Name of the WireGuard network interface";
    };

    externalInterface = lib.mkOption {
      type = lib.types.str;
      default = "enp7s0";
      description = "External network interface for WireGuard";
    };

    baseSubnet = lib.mkOption {
      type = lib.types.str;
      default = "10.100.0";
      description = "Base subnet for WireGuard network";
    };

    gateway = lib.mkOption {
      type = lib.types.str;
      default = "${cfg.baseSubnet}.1";
      description = "Gateway IP address for WireGuard network";
    };

    network = lib.mkOption {
      type = lib.types.str;
      default = "${cfg.baseSubnet}.0/24";
      description = "CIDR for WireGuard network";
    };

    listenPort = lib.mkOption {
      type = lib.types.int;
      default = 51820;
      description = "Port on which WireGuard listens";
    };
  };

  config = lib.mkIf cfg.enable {
    networking.firewall = {
      allowedUDPPorts = [ cfg.listenPort ];
    };

    sops.secrets.wireguard_key = {};
    networking.wireguard.interfaces = {

      ${cfg.networkName} = {
        # Determines the IP address and subnet of the server's end of the tunnel interface.
        ips = [ "${cfg.gateway}/24" ];

        listenPort = cfg.listenPort;
        # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
        # For this to work you have to set the dnsserver IP of your router (or dnsserver of choice) in your clients
        postSetup = ''
          ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s ${cfg.network} -o ${cfg.externalInterface} -j MASQUERADE
        '';

        # This undoes the above command
        postShutdown = ''
          ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s ${cfg.network} -o ${cfg.externalInterface} -j MASQUERADE
        '';

        privateKeyFile = config.sops.secrets.wireguard_key.path;

        peers = [
          {
            publicKey = "WiXR/lQSrTGf7fRkNGFqDGpr0f4h95ICwmvGu+NJQBA=";
            allowedIPs = [ "${cfg.baseSubnet}.3/32" ];
          }
          {
            publicKey = "+r85S5G10c3Ltc9NSMDzp3QOy0hT1pLm9Vb0cwmhxAE=";
            allowedIPs = [ "${cfg.baseSubnet}.5/32" ];
          }
        ];
      };
    };
  };
}

