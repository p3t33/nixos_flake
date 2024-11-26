{ config, lib, ... }:
let
  dnsPort = 53;
in
{
  # For dns port(53) accessed port need to be opend.
  networking.firewall.allowedTCPPorts = [ dnsPort ];
  networking.firewall.allowedUDPPorts = [ dnsPort ];

  # file generated at /var/lib/private/AdGuardHome/AdGuardHome.yaml
  services.adguardhome = {
    enable = true; # Enables AdGuard Home
    openFirewall = true; # Opens the firewall for AdGuard (optional)

    # Basic settings
    settings = {
      dns = {
        port = dnsPort; # DNS port for AdGuard
        upstream_dns = [
          "8.8.8.8"
          "8.8.4.4"
        ]; # Upstream DNS servers (Google DNS in this case)
      };

      filtering = {
                rewrites = []
          ++ lib.optionals config.services.deluge.enable [
            {
              domain = "deluge.${config.userDefinedGlobalVariables.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.syncthing.enable [
            {
              domain = "syncthing.${config.userDefinedGlobalVariables.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.adguardhome.enable [
            {
              domain = "adguard.${config.userDefinedGlobalVariables.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ];
      };
      http = {
        port = config.userDefinedGlobalVariables.servicePort.adguard;
      };
    };
  };
}