{ config, lib, hostSpecific, inputs, ... }:
let
  monitoring = "monitoring";
  files = "files";
  media = "media";
  devices = "devices";
  automation = "automation";
  appsDomain = config.custom.shared.appsDomain;
  hostIPv4Address = config.custom.shared.lan.hosts.${hostSpecific.hostName}.ipv4Address;
  automationHostIPv4Address = config.custom.shared.lan.hosts."home-assistant".ipv4Address;

in
{
  config = lib.mkIf config.services.homepage-dashboard.enable {
    # This file has all of the environment varibles that the systemd homepage-dashboard will be using
    # Note that all of the varibles names must start with HOMEPAGE_VAR_<your string>. So typical
    # secret file will have multiple lines and will look like:
    #
    # HOMEPAGE_VAR_DELUGE=<deluge web gui password>
    # HOMEPAGE_VAR_RADARR=<radarr api key>
    # HOMEPAGE_VAR_SONARR=<sonarr api key>
    sops.secrets.homepage-dashboard = {
        restartUnits = [ config.systemd.services.homepage-dashboard.name ];
    };

    services.homepage-dashboard = {
      listenPort = 8082;
      openFirewall = true;
      allowedHosts = "${config.custom.shared.localHostIPv4}:${toString config.services.homepage-dashboard.listenPort},${hostIPv4Address},${hostIPv4Address}:${toString config.services.homepage-dashboard.listenPort},homepage.${appsDomain}";
      environmentFiles = [ config.sops.secrets.homepage-dashboard.path ];


      widgets = [

        {
          resources = {
            cpu = true;
            disk = "/";
            memory = true;
            cputemp = true;
            uptime = true;
          };
        }

      ];
      services = [
        {
          "${files}" =
            [ ]
            ++ lib.optionals config.services.syncthing.enable [
              {
                "syncthing" = {
                  description = "real-time file synchronization";
                  href = "https://syncthing.${appsDomain}";
                  icon = "syncthing.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.custom.services.syncthing.httpPort}";
                  statusStyle = "dot";
                };
              }
            ]
            ++ lib.optionals config.services.deluge.enable [
              {
                "deluge" = {
                  description = "BitTorrent client";
                  href = "https://deluge.${appsDomain}";
                  icon = "deluge.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.deluge.web.port}";
                  statusStyle = "dot";
                  widget = {
                    type = "deluge";
                    url = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.deluge.web.port}";
                    password = "{{HOMEPAGE_VAR_DELUGE}}"; # not a hash, but human redable password used with the webgui.
                    enableLeechProgress = true;
                  };
                };
              }
            ]
            ++ lib.optionals config.services.qbittorrent.enable [
              {
                "qbittorrent" = {
                  description = "BitTorrent client";
                  href = "https://qbittorrent.${appsDomain}";
                  icon = "qbittorrent.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.qbittorrent.webuiPort}";
                  statusStyle = "dot";
                  widget = {
                    type = "qbittorrent";
                    url = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.qbittorrent.webuiPort}";
                    username = "{{HOMEPAGE_VAR_QBITTORRENT_USERNAME}}";
                    password = "{{HOMEPAGE_VAR_QBITTORRENT_PASSWORD}}";
                    enableLeechProgress = true;
                  };
                };
              }
            ]
            ++ lib.optionals config.services.sabnzbd.enable [
              {
                "sabnzbd" = {
                  description = "Usenet client";
                  href = "${if config.custom.security.acme.enable then "https" else "http"}://sabnzbd.${appsDomain}";
                  icon = "sabnzbd.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.custom.services.sabnzbd.httpPort}";
                  statusStyle = "dot";
                  widget = {
                      type = "sabnzbd";
                      url = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.custom.services.sabnzbd.httpPort}/sabnzbd";
                      key = "{{HOMEPAGE_VAR_SABNZBD}}";
                  };
                };
              }
            ]
            ++ lib.optionals config.services.paperless.enable [
              {
                "paperlessngx" = {
                  description = "Document management and OCR";
                  href = "https://paperless.${appsDomain}";
                  icon = "paperless.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.paperless.port}";
                  statusStyle = "dot";
                  # widget = {
                  #     type = "sabnzbd";
                  #     url = "http://${hostIPv4Address}:${builtins.toString config.custom.services.paperless.}/sabnzbd";
                  #     key = "{{HOMEPAGE_VAR_SABNZBD}}";
                  # };
                };
              }
            ];
        }
        {
          "${monitoring}" =
            [ ]
            ++ lib.optionals config.services.gatus.enable [
              {
                "gatus" = {
                  description = "Serivce health monitoring and alerting";
                  href = "https://gatus.${appsDomain}";
                  icon = "gatus.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.gatus.settings.web.port}/health";
                  statusStyle = "dot";
                  widget = {
                    type = "gatus";
                    url = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.gatus.settings.web.port}";
                  };
                };
              }
            ]
            ++ lib.optionals config.services.prometheus.enable [
              {
                "prometheus" = {
                  description = "Metrics collections and alerting";
                  href = "https://prometheus.${appsDomain}";
                  icon = "prometheus.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.prometheus.port}/-/ready";
                  statusStyle = "dot";
                  widget = {
                    type = "prometheus";
                    url = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.prometheus.port}";
                  };
                };
              }
            ]
            ++ lib.optionals config.services.grafana.enable [
              {
                "grafana" = {
                  description = "visualization and analytics platform";
                  href = "https://grafana.${appsDomain}";
                  icon = "grafana.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.grafana.settings.server.http_port}/api/health";
                  statusStyle = "dot";
                };
              }
            ];
        }
        {
          "${media}" =
            [ ]
            ++ lib.optionals config.services.sonarr.enable [
              {
                "sonarr" = {
                  description = "Tv series";
                  href = "${if config.custom.security.acme.enable then "https" else "http"}://sonarr.${appsDomain}${config.services.sonarr.settings.server.urlbase}";
                  icon = "sonarr.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.sonarr.settings.server.port}${config.services.sonarr.settings.server.urlbase}";
                  statusStyle = "dot";
                  widget = {
                      type = "sonarr";
                      url = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.sonarr.settings.server.port}${config.services.sonarr.settings.server.urlbase}";
                      key = "{{HOMEPAGE_VAR_SONARR}}";
                      enableQueue = true;
                  };
                };
              }
            ]
            ++ lib.optionals config.services.radarr.enable [
              {
                "radarr" = {
                  description = "Movies";
                  href = "${if config.custom.security.acme.enable then "https" else "http"}://radarr.${appsDomain}${config.services.radarr.settings.server.urlbase}/";
                  icon = "radarr.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.radarr.settings.server.port}${config.services.radarr.settings.server.urlbase}";
                  statusStyle = "dot";
                  widget = {
                    type = "radarr";
                    url = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.radarr.settings.server.port}${config.services.radarr.settings.server.urlbase}";
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
                  href = "${if config.custom.security.acme.enable then "https" else "http"}://prowlarr.${appsDomain}${config.services.prowlarr.settings.server.urlbase}/";
                  icon = "prowlarr.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.prowlarr.settings.server.port}${config.services.prowlarr.settings.server.urlbase}";
                  statusStyle = "dot";
                  widget = {
                    type = "prowlarr";
                    url = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.prowlarr.settings.server.port}${config.services.prowlarr.settings.server.urlbase}";
                    key = "{{HOMEPAGE_VAR_PROWLARR}}";
                    enableQueue = true;
                  };
                };
              }
            ]
            ++ lib.optionals config.services.jellyfin.enable [
              {
                "jellyfin" = {
                  description = "Media server";
                  href = "${if config.custom.security.acme.enable then "https" else "http"}://jellyfin.${appsDomain}/jellyfin/";
                  icon = "jellyfin.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.custom.servicePort.jellyfin}";
                  statusStyle = "dot";
                };
              }
            ]
            ++ lib.optionals config.services.immich.enable [
              {
                "immich" = {
                  description = "image service";
                  href = "${if config.custom.security.acme.enable then "https" else "http"}://immich.${appsDomain}";
                  icon = "immich.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.immich.port}";
                  statusStyle = "dot";
                  widget = {
                    type = "immich";
                    url = "http://${hostIPv4Address}:${builtins.toString config.services.immich.port}";
                    key = "{{HOMEPAGE_VAR_IMMICH}}";
                    version = 2;
                  };
                };
              }
            ]
            ++ lib.optionals config.services.calibre-web.enable [
              {
                "calibre-web" = {
                  description = "book library";
                  href = "https://calibre-web.${appsDomain}";
                  icon = "calibre-web.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.calibre-web.listen.port}";
                  statusStyle = "dot";
                  widget = {
                    type = "calibreweb";
                    url = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.calibre-web.listen.port}";
                    username = "{{HOMEPAGE_VAR_CALIBRE_USER}}";
                    password = "{{HOMEPAGE_VAR_CALIBRE_PASSWORD}}";
                  };
                };
              }
            ];
        }
        {
          "${devices}" = [
            {
              "router" = {
                description = "router ui";
                href = "https://router.home.medrish.com";
                icon = "router.png";
              };
            }
          ];
        }
        {
          "${automation}" =
            [ ]
            ++ lib.optionals config.services.n8n.enable [
              {
                "n8n" = {
                  description = "Serivce health monitoring and alerting";
                  href = "${if config.custom.security.acme.enable then "https" else "http"}://n8n.${appsDomain}";
                  icon = "n8n.png";
                  siteMonitor = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.custom.services.n8n.port}";
                  statusStyle = "dot";
                  # widget = {
                  #   type = "gatus";
                  #   url = "http://${hostIPv4Address}:${builtins.toString config.services.gatus.settings.web.port}";
                  # };
                };
              }
            ]
            ++ lib.optionals inputs.self.nixosConfigurations."home-assistant".config.services.home-assistant.enable [
              {
                "home-assistant" = {
                  description = "Home automation platform";
                  href = "${if config.custom.security.acme.enable then "https" else "http"}://home-assistant.${appsDomain}";
                  icon = "home-assistant.png";
                  siteMonitor = "http://${automationHostIPv4Address}:${builtins.toString inputs.self.nixosConfigurations."home-assistant".config.services.home-assistant.config.http.server_port}";
                  statusStyle = "dot";
                };
              }
            ]
            ++ lib.optionals inputs.self.nixosConfigurations."home-assistant".config.services.zigbee2mqtt.enable [
              {
                "zigbee2mqtt" = {
                  description = "Zigbee to MQTT bridge";
                  href = "${if config.custom.security.acme.enable then "https" else "http"}://zigbee2mqtt.${appsDomain}";
                  icon = "zigbee2mqtt.png";
                  siteMonitor = "http://${automationHostIPv4Address}:${builtins.toString inputs.self.nixosConfigurations."home-assistant".config.custom.servicePort.zigbee2mqttFrontend}";
                  statusStyle = "dot";
                };
              }
            ];
        }
      ];
      settings = {
        title = "nas Dashboard";
        # order of items in [] is the order of itmes on actual gui.
        layout = [
          { ${monitoring} = { style = "row"; columns = 3; }; }
          { ${files} = { style = "row"; columns = 3; }; }
          { ${media} = { style = "row"; columns = 3; }; }
          { ${automation} = { style = "row"; columns = 3; }; }
          { ${devices} = { style = "row"; columns = 3; }; }
        ];
      };
    };
  };
}
