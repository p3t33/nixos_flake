{ pkgs, config, ... }:
{

  networking.firewall = {
    allowedUDPPorts = [ config.userDefinedGlobalVariables.servicePort.wireguard ];
  };

  sops.secrets.wireguard_key = {};
  networking.wireguard.interfaces = {

    ${config.userDefinedGlobalVariables.wireguard.networkName} = {
      # Determines the IP address and subnet of the server's end of the tunnel interface.
      ips = [ "${config.userDefinedGlobalVariables.wireguard.gateway}/24" ];

      listenPort = config.userDefinedGlobalVariables.servicePort.wireguard;
      # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
      # For this to work you have to set the dnsserver IP of your router (or dnsserver of choice) in your clients
      postSetup = ''
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s ${config.userDefinedGlobalVariables.wireguard.network} -o ${config.userDefinedGlobalVariables.wireguard.externalInterface} -j MASQUERADE
      '';

      # This undoes the above command
      postShutdown = ''
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s ${config.userDefinedGlobalVariables.wireguard.network} -o ${config.userDefinedGlobalVariables.wireguard.externalInterface} -j MASQUERADE
      '';

      privateKeyFile = config.sops.secrets.wireguard_key.path;

      peers = [
        {
          publicKey = "z0h/OX6iwdFJxu0vTCb71NNvECUUiQPjY4+2ZmA+Pjo=";
          allowedIPs = [ "${config.userDefinedGlobalVariables.wireguard.baseSubnet}.2/32" ];
        }
      ];
    };
  };
}

