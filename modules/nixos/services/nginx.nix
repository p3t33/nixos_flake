{
  config,
  lib,
  hostSpecific,
  ...
}:
let
  httpPort = 80;
  httpsPort = 443;
  allInterfaces = "0.0.0.0";
  localHost = "http://${builtins.toString config.customGlobalOptions.localHostIPv4}";
in
{
  services.nginx = {
    enable = true;

    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;

    virtualHosts = {
      "${config.customGlobalOptions.${hostSpecific.hostName}.ip}" = {
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
              proxyPass = "${localHost}:${builtins.toString config.customOptions.syncthing.httpPort}/";
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
                proxy_set_header Host $host;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header X-Forwarded-Proto $scheme;
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
          // lib.optionalAttrs config.services.readarr.enable {
            "/readarr" = {
              proxyPass = "${localHost}:${builtins.toString config.services.readarr.settings.server.port}";
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
              proxyPass = "${localHost}:${builtins.toString config.customOptions.servicePort.sabnzbd}/sabnzbd/";
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
              proxyPass = "${localHost}:${builtins.toString config.customOptions.servicePort.jellyfin}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
            "/homepage/" =
              {
              }
              // lib.optionalAttrs config.services.homepage-dashboard.enable {
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
      "deluge.${hostSpecific.hostName}" =
        lib.optionalAttrs config.services.deluge.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.deluge.web.port}/";
              extraConfig = ''
                proxy_set_header X-Deluge-Base "/";
                add_header X-Frame-Options SAMEORIGIN;
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

      "sabnzbd.${hostSpecific.hostName}" = lib.optionalAttrs config.services.sabnzbd.enable {
        listen = [
        {
          addr = allInterfaces;
          port = httpPort;
        }
        ];
        locations."/" = {
          proxyPass = "${localHost}:${builtins.toString config.customOptions.servicePort.sabnzbd}/sabnzbd/";
          extraConfig = ''
          proxy_http_version 1.1;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection "upgrade";
          proxy_set_header X-Forwarded-Host $host;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          '';
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
              proxyPass = "${localHost}:${builtins.toString config.customOptions.syncthing.httpPort}/";
              extraConfig = ''
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
              '';
            };
          };

      # Add Sonarr Virtual Host
      "sonarr.${hostSpecific.hostName}" =
        lib.optionalAttrs config.services.sonarr.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.sonarr.settings.server.port}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
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

      "readarr.${hostSpecific.hostName}" =
        lib.optionalAttrs config.services.readarr.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.readarr.settings.server.port}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
          };

      "radarr.${hostSpecific.hostName}" =
        lib.optionalAttrs config.services.radarr.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.radarr.settings.server.port}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
          };

      "prowlarr.${hostSpecific.hostName}" =
        lib.optionalAttrs config.services.prowlarr.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.services.prowlarr.settings.server.port}";
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

    "calibre-web.${hostSpecific.hostName}" =
      lib.optionalAttrs config.services.calibre-web.enable
        {
          listen = [
            {
              addr = "${allInterfaces}";
              port = httpPort;
            }
          ];
          locations."/" = {
            proxyPass = "${localHost}:${builtins.toString config.services.calibre-web.listen.port}";

            extraConfig = ''
              proxy_http_version 1.1;
              proxy_set_header Host $host;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
              client_max_body_size 1024M;
            '';
          };
        };


      "jellyfin.${hostSpecific.hostName}" =
        lib.optionalAttrs config.services.jellyfin.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.customOptions.servicePort.jellyfin}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
          };

      "homepage.${hostSpecific.hostName}" = {
        listen = [
          {
            addr = "${allInterfaces}";
            port = httpPort;
          }
        ];
        # Serve homepage at the root of this domain
        locations."/" = {
          proxyPass = "${localHost}:${toString config.services.homepage-dashboard.listenPort}";
          extraConfig = ''
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
            proxy_redirect off;
          '';
        };
      };
      "prometheus.${hostSpecific.hostName}" = {
        listen = [
        {
            addr = "${allInterfaces}";
            port = httpPort;
        }
      ];
      locations."/" = {
      proxyPass = "${localHost}:${builtins.toString config.services.prometheus.port}/";
      extraConfig = ''
         proxy_http_version 1.1;
         proxy_set_header Upgrade $http_upgrade;
         proxy_set_header Connection "upgrade";
         proxy_redirect off;
         proxy_read_timeout 600s;
         proxy_send_timeout 600s;
      '';
      };
     };
    };
  };

  networking.firewall.allowedTCPPorts = [
    httpPort
    httpsPort
  ];
}
