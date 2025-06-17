{ config, lib, ... }:
{

  options.customOptions.wireguardQuickClient.networkName = lib.mkOption {
      default = "wg0";
      type = lib.types.str;
      description = "Defines value for wireguard client network";
  };

  config = {
    # As I did not want to put in plain text the url of the vpn service I am
    # using, I am putting the entire config into my sops file instead of
    # using a more declarative approach. The stracture inside of the secrets.yaml is:

    # wg-quick-client-config: |
    #   [Interface]
    #   PrivateKey = <private key of the client>
    #   Address = <the ip of the client, E.g: 10.100.0.4/24>
    #   DNS = <ip of private dns>, 1.1.1.1, 8.8.8.8 # this line is ooptional.
    #
    #   [Peer]
    #   PublicKey = <public key of the server>
    #   Endpoint = <ip/url of the server>:<port of the server>
    #   AllowedIPs = 0.0.0.0/0
    #   PersistentKeepalive = 25

    sops.secrets.wg-quick-client-config = {
        # wg-quick expects the file to be at a specific path, with specific name.
        path = "/etc/wireguard/${config.customOptions.wireguardQuickClient.networkName}.conf";
    };

    networking.wg-quick.interfaces.${config.customOptions.wireguardQuickClient.networkName} = {
      configFile = config.sops.secrets.wg-quick-client-config.path;
      autostart = false;  # Start manually
    };
  };
}

