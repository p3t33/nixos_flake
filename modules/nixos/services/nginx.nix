{
  config,
  lib,
  hostSpecific,
  inputs,
  ...
}:
let

  httpPort = 80;
  httpsPort = 443;
  allInterfaces = "0.0.0.0";
  localHost = "http://${builtins.toString config.custom.shared.localHostIPv4}";
  appsDomain = config.custom.shared.appsDomain;
  automationConfig = inputs.self.nixosConfigurations."home-assistant".config;
  automationHostIPv4 = automationConfig.custom.shared."home-assistant".ip;
in
{
  config = lib.mkIf config.services.nginx.enable {
    services.nginx = {
      recommendedProxySettings = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;

      virtualHosts = {
        "${config.custom.shared.${hostSpecific.hostName}.ip}" = {
          listen = [
            {
              addr = "${allInterfaces}"; # Listen on all available network interfaces
              port = httpPort; # HTTP port
            }
          ];

          # Conditional locations based on enabled services
          locations = lib.recursiveUpdate { } (
            lib.optionalAttrs config.services.syncthing.enable {
              "/syncthing/" = {
                proxyPass = "${localHost}:${builtins.toString config.custom.services.syncthing.httpPort}/";
                extraConfig = ''
                  proxy_read_timeout 600s;
                  proxy_send_timeout 600s;
                '';
              };
            }
            // lib.optionalAttrs config.services.adguardhome.enable {
              "/adguard/" = {
                proxyPass = "${localHost}:${builtins.toString config.services.adguardhome.port}/";
              };
            }
            // lib.optionalAttrs config.services.deluge.enable {
              "/deluge/" = {
                proxyPass = "${localHost}:${builtins.toString config.services.deluge.web.port}/";
                extraConfig = ''
                  proxy_set_header X-Deluge-Base "/deluge/";
                  add_header X-Frame-Options SAMEORIGIN;
                '';
              };
            }
            // lib.optionalAttrs config.services.sonarr.enable {
              "/sonarr" = {
                proxyPass = "${localHost}:${builtins.toString config.services.sonarr.settings.server.port}";
                extraConfig = ''
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection "upgrade";
                  proxy_redirect off;
                '';
              };
            }

            // lib.optionalAttrs config.services.calibre-web.enable {
              "/calibre-web/" = {
                proxyPass = "${localHost}:${builtins.toString config.services.calibre-web.listen.port}";
                proxyWebsockets = true;
                extraConfig = ''
                  proxy_set_header X-Script-Name /calibre-web;
                  client_max_body_size 1024M;
               '';
              };
            }
            // lib.optionalAttrs config.services.bazarr.enable {
              "/bazarr" = {
                proxyPass = "${localHost}:${builtins.toString config.services.bazarr.listenPort}";
                extraConfig = ''
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection "upgrade";
                  proxy_redirect off;
                '';
              };
            }
            // lib.optionalAttrs config.services.radarr.enable {
              "/radarr" = {
                proxyPass = "${localHost}:${builtins.toString config.services.radarr.settings.server.port}";
                extraConfig = ''
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection "upgrade";
                  proxy_redirect off;
                '';
              };
            }
            // lib.optionalAttrs config.services.prowlarr.enable {
              "/prowlarr" = {
                proxyPass = "${localHost}:${builtins.toString config.services.prowlarr.settings.server.port}";
                extraConfig = ''
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection "upgrade";
                  proxy_redirect off;
                '';
              };
            }
            // lib.optionalAttrs config.services.jackett.enable {
              "/jackett" = {
                proxyPass = "${localHost}:${builtins.toString config.services.jackett.port}";
                extraConfig = ''
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection "upgrade";
                  proxy_redirect off;
                '';
              };
            }
            // lib.optionalAttrs config.services.sabnzbd.enable {
              "/sabnzbd/" = {
                proxyPass = "${localHost}:${builtins.toString config.custom.services.sabnzbd.httpPort}/sabnzbd/";
                extraConfig = ''
                  proxy_set_header X-Forwarded-Host $host;
                  proxy_set_header X-Forwarded-Server $host;
                  proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection "upgrade";
                '';
              };
            }

            // lib.optionalAttrs config.services.jellyfin.enable {
              "/jellyfin" = {
                proxyPass = "${localHost}:${builtins.toString config.custom.servicePort.jellyfin}";
                extraConfig = ''
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection "upgrade";
                  proxy_redirect off;
                '';
              };
            }
            // lib.optionalAttrs config.services.homepage-dashboard.enable {
              "/homepage/" = {
                proxyPass = "${localHost}:${builtins.toString config.services.homepage-dashboard.listenPort}/";
                extraConfig = ''
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection "upgrade";
                  proxy_redirect off;
                '';
              };
            }
          );
        };

        # Conditionally add virtual hosts based on enabled services
        "qbittorrent.${hostSpecific.hostName}" =
          lib.optionalAttrs config.services.qbittorrent.enable
            {
              listen = [
                {
                  addr = "${allInterfaces}";
                  port = httpPort;
                }
              ];
              locations."/" = {
                recommendedProxySettings = false;
                proxyPass = "${localHost}:${builtins.toString config.services.qbittorrent.webuiPort}/";
                extraConfig = ''
                  proxy_http_version 1.1;
                  proxy_set_header Host $proxy_host;
                  proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                  proxy_set_header X-Forwarded-Host $http_host;
                  proxy_set_header X-Forwarded-Proto $scheme;
                '';
              };
            };

        "adguard.${hostSpecific.hostName}" =
          lib.optionalAttrs config.services.adguardhome.enable
            {
              listen = [
                {
                  addr = "${allInterfaces}";
                  port = httpPort;
                }
              ];
              locations."/" = {
                proxyPass = "${localHost}:${builtins.toString config.services.adguardhome.port}/";
              };
            };

        "syncthing.${hostSpecific.hostName}" =
          lib.optionalAttrs config.services.syncthing.enable
            {
              listen = [
                {
                  addr = "${allInterfaces}";
                  port = httpPort;
                }
              ];
              locations."/" = {
                proxyPass = "${localHost}:${builtins.toString config.custom.services.syncthing.httpPort}/";
                extraConfig = ''
                  proxy_read_timeout 600s;
                  proxy_send_timeout 600s;
                '';
              };
            };

        "bazarr.${hostSpecific.hostName}" =
          lib.optionalAttrs config.services.bazarr.enable
            {
              listen = [
                {
                  addr = "${allInterfaces}";
                  port = httpPort;
                }
              ];
              locations."/" = {
                proxyPass = "${localHost}:${builtins.toString config.services.bazarr.listenPort}";
                extraConfig = ''
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection "upgrade";
                  proxy_redirect off;
                '';
              };
            };

        "jackett.${hostSpecific.hostName}" =
          lib.optionalAttrs config.services.jackett.enable
            {
              listen = [
                {
                  addr = "${allInterfaces}";
                  port = httpPort;
                }
              ];
              locations."/" = {
                proxyPass = "${localHost}:${builtins.toString config.services.jackett.port}";
                extraConfig = ''
                      proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection "upgrade";
                  proxy_redirect off;
                '';
              };
            };

        "homepage.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.homepage-dashboard.listenPort}/";
              proxyWebsockets = true;
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };

      }
      // lib.optionalAttrs config.services.syncthing.enable {
         "syncthing.${appsDomain}" =
           {
             locations."/" = {
               proxyPass = "${localHost}:${builtins.toString config.custom.services.syncthing.httpPort}/";
               extraConfig = ''
                 proxy_read_timeout 600s;
                 proxy_send_timeout 600s;
               '';
             };
           }
           // lib.optionalAttrs config.custom.security.acme.enable {
             useACMEHost = appsDomain;
             forceSSL = true;
           };
       }
      // lib.optionalAttrs config.services.prometheus.enable {
        "prometheus.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.prometheus.port}/";
              proxyWebsockets = true;
              extraConfig = ''
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.grafana.enable {
        "grafana.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.grafana.settings.server.http_port}/";
              proxyWebsockets = true;
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.gatus.enable {
        "gatus.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.gatus.settings.web.port}/";
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.paperless.enable {
        "paperless.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.paperless.port}/";
              proxyWebsockets = true;
              extraConfig = ''
                client_max_body_size 100M;
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
              '' + lib.optionalString config.custom.security.acme.enable ''
                proxy_cookie_flags ~ secure;
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.qbittorrent.enable {
        "qbittorrent.${appsDomain}" =
          {
            locations."/" = {
              recommendedProxySettings = false;
              proxyPass = "${localHost}:${builtins.toString config.services.qbittorrent.webuiPort}/";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Host $proxy_host;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header X-Forwarded-Host $http_host;
                proxy_set_header X-Forwarded-Proto $scheme;
                client_max_body_size 100M;
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.deluge.enable {
        "deluge.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.deluge.web.port}/";
              extraConfig = ''
                proxy_set_header X-Deluge-Base "/";
                add_header X-Frame-Options SAMEORIGIN;
                client_max_body_size 100M;
                proxy_cookie_flags _session_id ${if config.custom.security.acme.enable then "secure httponly samesite=lax" else "httponly samesite=lax"};
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.calibre-web.enable {
        "calibre-web.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.calibre-web.listen.port}/";
              extraConfig = ''
                proxy_set_header X-Scheme $scheme;
                proxy_hide_header Strict-Transport-Security;
                client_max_body_size 1024M;
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
              '' + lib.optionalString config.custom.security.acme.enable ''
                proxy_cookie_flags ~ secure;
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.sabnzbd.enable {
        "sabnzbd.${appsDomain}" =
          {
            locations =
              let
                sabnzbdProxy = {
                  proxyPass = "${localHost}:${builtins.toString config.custom.services.sabnzbd.httpPort}/sabnzbd/";
                  extraConfig = ''
                    proxy_http_version 1.1;
                    proxy_set_header Connection "";
                    proxy_redirect /sabnzbd/ /;
                    proxy_redirect http:// $scheme://;
                    client_max_body_size 100M;
                    proxy_read_timeout 600s;
                    proxy_send_timeout 600s;
                  '';
                };
              in
              {
                "/" = sabnzbdProxy;
                "/sabnzbd" = sabnzbdProxy // {
                  proxyPass = "${localHost}:${builtins.toString config.custom.services.sabnzbd.httpPort}/sabnzbd";
                };
              };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.sonarr.enable {
        "sonarr.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.sonarr.settings.server.port}";
              proxyWebsockets = true;
              extraConfig = ''
                proxy_redirect off;
                client_max_body_size 100M;
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.radarr.enable {
        "radarr.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.radarr.settings.server.port}";
              proxyWebsockets = true;
              extraConfig = ''
                proxy_redirect off;
                client_max_body_size 100M;
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.prowlarr.enable {
        "prowlarr.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.prowlarr.settings.server.port}";
              proxyWebsockets = true;
              extraConfig = ''
                proxy_redirect off;
                client_max_body_size 100M;
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.jellyfin.enable {
        "jellyfin.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.custom.servicePort.jellyfin}";
              proxyWebsockets = true;
              extraConfig = ''
                proxy_redirect off;
                proxy_buffering off;
                client_max_body_size 20M;
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.immich.enable {
        "immich.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.immich.port}";
              proxyWebsockets = true;
              extraConfig = ''
                proxy_redirect off;
                client_max_body_size 50000M;
                proxy_request_buffering off;
                client_body_buffer_size 1024k;
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
                send_timeout 600s;
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs config.services.n8n.enable {
        "n8n.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.custom.services.n8n.port}";
              proxyWebsockets = true;
              extraConfig = ''
                proxy_redirect off;
                proxy_buffering off;
                proxy_request_buffering off;
                client_max_body_size 256M;
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
                send_timeout 600s;
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs automationConfig.services.home-assistant.enable {
        "home-assistant.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "http://${automationHostIPv4}:${builtins.toString automationConfig.services.home-assistant.config.http.server_port}";
              proxyWebsockets = true;
              extraConfig = ''
                proxy_redirect off;
                proxy_buffering off;
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      }
      // lib.optionalAttrs automationConfig.services.zigbee2mqtt.enable {
        "zigbee2mqtt.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "http://${automationHostIPv4}:${builtins.toString automationConfig.custom.servicePort.zigbee2mqttFrontend}/";
              proxyWebsockets = true;
              extraConfig = ''
                proxy_redirect off;
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
              '';
            };
          }
          // lib.optionalAttrs config.custom.security.acme.enable {
            useACMEHost = appsDomain;
            forceSSL = true;
          };
      };
    };

    networking.firewall.allowedTCPPorts = [
      httpPort
      httpsPort
    ];
  };
}
