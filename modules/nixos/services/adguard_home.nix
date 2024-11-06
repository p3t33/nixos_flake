let
  dnsPort = 53;
  httPort = 3000;
in
{
  # For dns port(53) accessed port need to be opend.
  networking.firewall.allowedTCPPorts = [ dnsPort ];
  networking.firewall.allowedUDPPorts = [ dnsPort ];

  services.adguardhome = {
    enable = true;                  # Enables AdGuard Home
    openFirewall = true;             # Opens the firewall for AdGuard (optional)

    # Basic settings
    settings = {
      dns = {
        port = dnsPort;                  # DNS port for AdGuard
        upstream_dns = [ "8.8.8.8" "8.8.4.4" ]; # Upstream DNS servers (Google DNS in this case)
      };
      http = {
        port = httPort;                # Web interface port, can be changed
      };
    };
  };
}
