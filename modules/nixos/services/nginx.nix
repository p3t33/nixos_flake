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
  hostIPv4Address = config.custom.shared.lan.hosts.${hostSpecific.hostName}.ipv4Address;
  automationConfig = inputs.self.nixosConfigurations."home-assistant".config;
  automationHostIPv4 = config.custom.shared.lan.hosts."home-assistant".ipv4Address;
in
{
  config = lib.mkIf config.services.nginx.enable {
    services.nginx = {
      recommendedProxySettings = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;

      virtualHosts = {
        "${hostIPv4Address}" = {
          default = true;
          listen = [
            {
              addr = allInterfaces;
              port = httpPort;
            }
          ];
          locations."/" = {
            return = 410;
            extraConfig = ''
              add_header Cache-Control "no-store" always;
            '';
          };
        };

      }
      // lib.optionalAttrs config.services.homepage-dashboard.enable {
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
      // lib.optionalAttrs config.services.adguardhome.enable {
        "adguard.${appsDomain}" =
          {
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.adguardhome.port}/";
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
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.custom.services.sabnzbd.httpPort}/";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Connection "";
                proxy_redirect http:// $scheme://;
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
