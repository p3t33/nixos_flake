{ config, lib, hostSpecific, ... }:
let
  dnsPort = 53;
in
{

  config = lib.mkIf config.services.adguardhome.enable {
    # For dns port(53) accessed port need to be opend.
    networking.firewall.allowedTCPPorts = [ dnsPort ];
    networking.firewall.allowedUDPPorts = [ dnsPort ];

    # file generated at /var/lib/private/AdGuardHome/AdGuardHome.yaml
    services.adguardhome = {
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
                domain = "deluge.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.syncthing.enable [
              {
                domain = "syncthing.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.adguardhome.enable [
              {
                domain = "adguard.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.sonarr.enable [
              {
                domain = "sonarr.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.bazarr.enable [
              {
                domain = "bazarr.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.radarr.enable [
              {
                domain = "radarr.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.prowlarr.enable [
              {
                domain = "prowlarr.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.jackett.enable [
              {
                domain = "jackett.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.jellyfin.enable [
              {
                domain = "jellyfin.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.calibre-web.enable [
              {
                domain = "calibre-web.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.homepage-dashboard.enable [
              {
                domain = "homepage.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.prometheus.enable [
              {
                domain = "prometheus.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.sabnzbd.enable [
              {
                domain = "sabnzbd.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ]
            ++ lib.optionals config.services.grafana.enable [
              {
                domain = "grafana.${hostSpecific.hostName}";
                answer = "${config.customGlobal.${hostSpecific.hostName}.ip}";
              }
            ];
        };
      };
    };
  };
}
