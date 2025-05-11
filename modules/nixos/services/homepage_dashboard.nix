{ config, lib, ... }:
{

  services.homepage-dashboard = {
    enable = true; # Enable the service
    listenPort = config.userDefinedGlobalVariables.servicePort.homepageDashboard;
    openFirewall = true; # Open the firewall for the port
    widgets = [

      {
        resources = {
          cpu = true;
          disk = "/";
          memory = true;
        };
      }

    ];
    services = [
      {
        "media" =
          [ ]
          ++ lib.optionals config.services.deluge.enable [
            {
              "deluge" = {
                description = "BitTorrent client";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/deluge"; # URL to the Deluge service
                icon = "deluge.png"; # Icon for Deluge
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.deluge.webGUI}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.sabnzbd.enable [
            {
              "sabnzbd" = {
                description = "Usenet client";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/sabnzbd"; # URL to the Deluge service
                icon = "sabnzbd.png"; # Icon for Deluge
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.sabnzbd}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.bazarr.enable [
            {
              "bazarr" = {
                description = "Subtitles for media library";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/bazarr"; # URL to the Deluge service
                icon = "bazarr.png"; # Icon for Deluge
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.bazarr}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.sonarr.enable [
            {
              "sonarr" = {
                description = "Tv series";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/sonarr"; # URL to the Sonarr service
                icon = "sonarr.png"; # Icon for Sonarr
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.sonarr}";
                statusStyle = "dot";
                widget = lib.mkIf config.services.prometheus.exporters.exportarr-sonarr.enable {
                  type = "prometheus";
                  url =  "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.prometheus.server}";
                  query = "sonarr_queue_total{instance=\"${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.prometheus.sonarrExporter}\"}";
                };
              };
            }
          ]
          ++ lib.optionals config.services.readarr.enable [
            {
              "readarr" = {
                description = "Ebooks";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/readarr"; # URL to the Sonarr service
                icon = "readarr.png"; # Icon for Sonarr
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.readarr}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.radarr.enable [
            {
              "radarr" = {
                description = "Movies";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/radarr"; # URL to the Radarr service
                icon = "radarr.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.radarr}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.prowlarr.enable [
            {
              "prowlarr" = {
                description = "Indexer manager";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/prowlarr"; # URL to the Prowlarr service
                icon = "prowlarr.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.prowlarr}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.jackett.enable [
            {
              "jackett" = {
                description = "Indexer manager";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/jackett"; # URL to the Jackett service
                icon = "jackett.png"; # Icon for Jackett
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.jackett}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.jellyfin.enable [
            {
              "jellyfin" = {
                description = "Media server";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/jellyfin"; # URL to the Jellyfin service
                icon = "jellyfin.png"; # Icon for Jellyfin
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.jellyfin}";
                statusStyle = "dot";
              };
            }
          ];
      }
      {
        "monitoring" =
          [ ]
          ++ lib.optionals config.services.prometheus.enable [
            {
              "prometheus" = {
                description = "Metrics collections and alerting";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}:${builtins.toString config.userDefinedGlobalVariables.servicePort.prometheus.server}"; # Convert port to string
                icon = "prometheus.png"; # Icon for Deluge
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.prometheus.server}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.grafana.enable [
            {
              "grafana" = {
                description = "visualization and analytics platform";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}:${builtins.toString config.userDefinedGlobalVariables.servicePort.grafana}"; # Convert port to string
                icon = "grafana.png"; # Icon for Deluge
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.grafana}";
                statusStyle = "dot";
              };
            }
          ];
      }
      {
        "files" =
          [ ]
          ++ lib.optionals config.services.syncthing.enable [
            {
              "syncthing" = {
                description = "real-time file synchronization";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/syncthing"; # URL to the Syncthing service
                icon = "syncthing.png"; # Icon for Syncthing
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.syncthing}";
                statusStyle = "dot";
              };
            }
          ];
      }
      {
        "filtering" =
          [ ]
          ++ lib.optionals config.services.adguardhome.enable [
            {
              "adguard" = {
                description = "DNS based ad blocker";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/adguard"; # URL to the AdGuard service
                icon = "adguard-home.png"; # Icon for AdGuard
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.adguard}";
                statusStyle = "dot";
              };
            }
          ];
      }
      {
        "devices" = [
          {
            "router" = {
              description = "router ui";
              href = "http://${config.userDefinedGlobalVariables.routerIP}"; # URL to the AdGuard service
              icon = "router.png"; # Icon for AdGuard
            };
          }
          {
            "proxmox" = {
              description = "Proxmox VE Dashboard";
              href = "http://${config.userDefinedGlobalVariables.proxmoxIP}:8006"; # URL to the Proxmox interface
              icon = "proxmox.png";
            };
          }
        ];
      }
    ];
    settings = {
      # base = "http://homepage.homelab";
      title = "My Dashboard"; # Set the title of your homepage
    };
  };
}
