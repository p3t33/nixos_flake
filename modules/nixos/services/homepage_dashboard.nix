{ config, lib, ... }:
{
  # /etc/homepage-dashboard/services.yaml
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
                description = "Deluge Web UI";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/deluge"; # URL to the Deluge service
                icon = "deluge.png"; # Icon for Deluge
              };
            }
          ]
          ++ lib.optionals config.services.sonarr.enable [
            {
              "sonarr" = {
                description = "Sonarr Web UI";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/sonarr"; # URL to the Sonarr service
                icon = "sonarr.png"; # Icon for Sonarr
              };
            }
          ]
          ++ lib.optionals config.services.radarr.enable [
            {
              "radarr" = {
                description = "Radarr Web UI";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/radarr"; # URL to the Radarr service
                icon = "radarr.png"; # Icon for Radarr
              };
            }
          ]
          ++ lib.optionals config.services.prowlarr.enable [
            {
              "prowlarr" = {
                description = "Prowlarr Web UI";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/prowlarr"; # URL to the Prowlarr service
                icon = "prowlarr.png"; # Icon for Prowlarr
              };
            }
          ]
          ++ lib.optionals config.services.jackett.enable [
            {
              "jackett" = {
                description = "Jackett Web UI";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/jackett"; # URL to the Jackett service
                icon = "jackett.png"; # Icon for Jackett
              };
            }
          ]
          ++ lib.optionals config.services.jellyfin.enable [
            {
              "jellyfin" = {
                description = "Jellyfin Web UI";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/jellyfin"; # URL to the Jellyfin service
                icon = "jellyfin.png"; # Icon for Jellyfin
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
                description = "Syncthing Web GUI";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/syncthing"; # URL to the Syncthing service
                icon = "syncthing.png"; # Icon for Syncthing
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
                description = "AdGuard Web GUI";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/adguard"; # URL to the AdGuard service
                icon = "adguard-home.png"; # Icon for AdGuard
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
