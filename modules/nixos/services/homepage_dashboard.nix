{ config, lib, ... }:
{

  # This file has all of the environment varibles that the systemd homepage-dashboard will be using
  # Note that all of the varibles names must start with HOMEPAGE_VAR_<your string>. So typical
  # secret file will have multiple lines and will look like:
  #
  # HOMEPAGE_VAR_DELUGE=<deluge web gui password>
  # HOMEPAGE_VAR_RADARR=<radarr api key>
  # HOMEPAGE_VAR_SONARR=<sonarr api key>
  sops.secrets.homepage-dashboard = {};

  services.homepage-dashboard = {
    enable = true;
    listenPort = 8082;
    openFirewall = true;
    allowedHosts = "${config.userDefinedGlobalVariables.localHostIPv4},${config.userDefinedGlobalVariables.anyIPv4},homepage.homelab,${config.userDefinedGlobalVariables.homeLabIP}";
    environmentFile = config.sops.secrets.homepage-dashboard.path;


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
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/deluge";
                icon = "deluge.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.services.deluge.web.port}";
                statusStyle = "dot";
                widget = {
                  type = "deluge";
                  url = "http://${config.userDefinedGlobalVariables.homeLabIP}:${builtins.toString config.services.deluge.web.port}";
                  password = "{{HOMEPAGE_VAR_DELUGE}}"; # not a hash, but human redable password used with the webgui.
                  enableLeechProgress = true;
                };
              };
            }
          ]
          ++ lib.optionals config.services.sabnzbd.enable [
            {
              "sabnzbd" = {
                description = "Usenet client";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/sabnzbd";
                icon = "sabnzbd.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.sabnzbd}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.bazarr.enable [
            {
              "bazarr" = {
                description = "Subtitles for media library";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/bazarr";
                icon = "bazarr.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.services.bazarr.listenPort}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.sonarr.enable [
            {
              "sonarr" = {
                description = "Tv series";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/sonarr";
                icon = "sonarr.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.services.sonarr.settings.server.port}";
                statusStyle = "dot";
                widget = {
                    type = "sonarr";
                    url = "http://${config.userDefinedGlobalVariables.homeLabIP}:${builtins.toString config.services.sonarr.settings.server.port}";
                    key = "{{HOMEPAGE_VAR_SONARR}}";
                    enableQueue = true;
                };
              };
            }
          ]
          ++ lib.optionals config.services.calibre-web.enable [
            {
              "calibre-web" = {
                description = "book library";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/calibre-web";
                icon = "calibre-web.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.services.calibre-web.listen.port}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.readarr.enable [
            {
              "readarr" = {
                description = "Ebooks";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/readarr";
                icon = "readarr.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.services.readarr.settings.server.port}";
                statusStyle = "dot";
                widget = {
                  type = "readarr";
                  url = "http://${config.userDefinedGlobalVariables.homeLabIP}:${builtins.toString config.services.readarr.settings.server.port}";
                  key = "{{HOMEPAGE_VAR_READARR}}"; #
                  enableQueue = true;
                };
              };
            }
          ]
          ++ lib.optionals config.services.radarr.enable [
            {
              "radarr" = {
                description = "Movies";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/radarr";
                icon = "radarr.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.services.radarr.settings.server.port}";
                statusStyle = "dot";
                widget = {
                  type = "radarr";
                  url = "http://${config.userDefinedGlobalVariables.homeLabIP}:${builtins.toString config.services.radarr.settings.server.port}";
                  key = "{{HOMEPAGE_VAR_RADARR}}";
                  enableQueue = true;
                };
              };
            }
          ]
          ++ lib.optionals config.services.prowlarr.enable [
            {
              "prowlarr" = {
                description = "Indexer manager";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/prowlarr";
                icon = "prowlarr.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.services.prowlarr.settings.server.port}";
                statusStyle = "dot";
                widget = {
                  type = "prowlarr";
                  url = "http://${config.userDefinedGlobalVariables.homeLabIP}:${builtins.toString config.services.prowlarr.settings.server.port}";
                  key = "{{HOMEPAGE_VAR_PROWLARR}}";
                  enableQueue = true;
                };
              };
            }
          ]
          ++ lib.optionals config.services.jackett.enable [
            {
              "jackett" = {
                description = "Indexer manager";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/jackett";
                icon = "jackett.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.services.jackett.port}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.jellyfin.enable [
            {
              "jellyfin" = {
                description = "Media server";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/jellyfin";
                icon = "jellyfin.png";
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
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}:${builtins.toString config.services.prometheus.port}";
                icon = "prometheus.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.services.prometheus.port}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.grafana.enable [
            {
              "grafana" = {
                description = "visualization and analytics platform";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}:${builtins.toString config.services.grafana.settings.server.http_port}";
                icon = "grafana.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.services.grafana.settings.server.http_port}";
                statusStyle = "dot";
              };
            }
          ]
          ++ lib.optionals config.services.gatus.enable [
            {
              "gatus" = {
                description = "Serivce health monitoring and alerting";
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}:${builtins.toString config.services.gatus.settings.web.port}";
                icon = "gatus.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.services.gatus.settings.web.port}";
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
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/syncthing";
                icon = "syncthing.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.syncthing.httpPort}";
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
                href = "http://${config.userDefinedGlobalVariables.homeLabIP}/adguard";
                icon = "adguard-home.png";
                siteMonitor = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.services.adguardhome.port}";
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
              href = "http://${config.userDefinedGlobalVariables.routerIP}";
              icon = "router.png";
            };
          }
          {
            "proxmox" = {
              description = "Proxmox VE Dashboard";
              href = "http://${config.userDefinedGlobalVariables.proxmoxIP}:8006";
              icon = "proxmox.png";
            };
          }
        ];
      }
    ];
    settings = {
      title = "homelab Dashboard";
    };
  };
}
