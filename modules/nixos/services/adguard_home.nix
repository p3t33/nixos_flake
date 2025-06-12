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
    port = 3000;

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
        rewrites =
          [ ]
          ++ lib.optionals config.services.deluge.enable [
            {
              domain = "deluge.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.syncthing.enable [
            {
              domain = "syncthing.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.adguardhome.enable [
            {
              domain = "adguard.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.sonarr.enable [
            {
              domain = "sonarr.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.bazarr.enable [
            {
              domain = "bazarr.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.readarr.enable [
            {
              domain = "readarr.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.radarr.enable [
            {
              domain = "radarr.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.prowlarr.enable [
            {
              domain = "prowlarr.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.jackett.enable [
            {
              domain = "jackett.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.jellyfin.enable [
            {
              domain = "jellyfin.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.calibre-web.enable [
            {
              domain = "calibre-web.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.homepage-dashboard.enable [
            {
              domain = "homepage.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.prometheus.enable [
            {
              domain = "prometheus.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.sabnzbd.enable [
            {
              domain = "sabnzbd.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ]
          ++ lib.optionals config.services.grafana.enable [
            {
              domain = "grafana.${config.hostSpecification.hostConfigurationName}";
              answer = "${config.userDefinedGlobalVariables.homeLabIP}";
            }
          ];
      };
    };
  };
}
