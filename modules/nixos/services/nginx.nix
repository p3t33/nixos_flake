{
  config,
  lib,
  ...
}:
let
  httpPort = 80;
  httpsPort = 443;
  allInterfaces = "0.0.0.0";
  localHost = "http://${builtins.toString config.userDefinedGlobalVariables.localHostIPv4}";
in
{
  services.nginx = {
    enable = true;

    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;

    virtualHosts = {
      "${config.userDefinedGlobalVariables.homeLabIP}" = {
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
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.syncthing}/";
              extraConfig = ''
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
              '';
            };
          }
          // lib.optionalAttrs config.services.adguardhome.enable {
            "/adguard/" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.adguard}/";
            };
          }
          // lib.optionalAttrs config.services.deluge.enable {
            "/deluge/" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.deluge.webGUI}/";
              extraConfig = ''
                proxy_set_header X-Deluge-Base "/deluge/";
                add_header X-Frame-Options SAMEORIGIN;
              '';
            };
          }
          // lib.optionalAttrs config.services.sonarr.enable {
            "/sonarr" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.sonarr}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
          }
          // lib.optionalAttrs config.services.bazarr.enable {
            "/bazarr" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.bazarr}";
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
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.readarr}";
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
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.radarr}";
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
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.prowlarr}";
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
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.jackett}";
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
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.sabnzbd}/sabnzbd/";
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
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.jellyfin}";
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
                proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.homepageDashboard}/";
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
      "deluge.${config.userDefinedGlobalVariables.hostConfigurationName}" =
        lib.optionalAttrs config.services.deluge.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.deluge.webGUI}/";
              extraConfig = ''
                proxy_set_header X-Deluge-Base "/";
                add_header X-Frame-Options SAMEORIGIN;
              '';
            };
          };

      "adguard.${config.userDefinedGlobalVariables.hostConfigurationName}" =
        lib.optionalAttrs config.services.adguardhome.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.adguard}/";
            };
          };

      "sabnzbd.${config.userDefinedGlobalVariables.hostConfigurationName}" = lib.optionalAttrs config.services.sabnzbd.enable {
        listen = [
        {
          addr = allInterfaces;
          port = httpPort;
        }
        ];
        locations."/" = {
          proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.sabnzbd}/sabnzbd/";
          extraConfig = ''
          proxy_http_version 1.1;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection "upgrade";
          proxy_set_header X-Forwarded-Host $host;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          '';
        };
      };


      "syncthing.${config.userDefinedGlobalVariables.hostConfigurationName}" =
        lib.optionalAttrs config.services.syncthing.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.syncthing}/";
              extraConfig = ''
                proxy_read_timeout 600s;
                proxy_send_timeout 600s;
              '';
            };
          };

      # Add Sonarr Virtual Host
      "sonarr.${config.userDefinedGlobalVariables.hostConfigurationName}" =
        lib.optionalAttrs config.services.sonarr.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.sonarr}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
          };

      "bazarr.${config.userDefinedGlobalVariables.hostConfigurationName}" =
        lib.optionalAttrs config.services.bazarr.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.bazarr}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
          };

      "readarr.${config.userDefinedGlobalVariables.hostConfigurationName}" =
        lib.optionalAttrs config.services.readarr.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.readarr}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
          };

      "radarr.${config.userDefinedGlobalVariables.hostConfigurationName}" =
        lib.optionalAttrs config.services.radarr.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.radarr}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
          };

      "prowlarr.${config.userDefinedGlobalVariables.hostConfigurationName}" =
        lib.optionalAttrs config.services.prowlarr.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.prowlarr}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
          };

      "jackett.${config.userDefinedGlobalVariables.hostConfigurationName}" =
        lib.optionalAttrs config.services.jackett.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.jackett}";
              extraConfig = ''
                    proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
          };

      "jellyfin.${config.userDefinedGlobalVariables.hostConfigurationName}" =
        lib.optionalAttrs config.services.jellyfin.enable
          {
            listen = [
              {
                addr = "${allInterfaces}";
                port = httpPort;
              }
            ];
            locations."/" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.jellyfin}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
          };

      "homepage.${config.userDefinedGlobalVariables.hostConfigurationName}" = {
        listen = [
          {
            addr = "${allInterfaces}";
            port = httpPort;
          }
        ];
        # Serve homepage at the root of this domain
        locations."/" = {
          proxyPass = "${localHost}:${toString config.userDefinedGlobalVariables.servicePort.homepageDashboard}";
          extraConfig = ''
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
            proxy_redirect off;
          '';
        };
      };
      "prometheus.${config.userDefinedGlobalVariables.hostConfigurationName}" = {
        listen = [
        {
            addr = "${allInterfaces}";
            port = httpPort;
        }
      ];
      locations."/" = {
      proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.prometheus.server}/";
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
