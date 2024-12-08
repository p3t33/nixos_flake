{ config, pkgs, lib, ... }:
let
  httpPort = 80;
  httpsPort = 443;
  allInterfaces = "0.0.0.0";
  localHost = "http://localhost";
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
        locations = lib.recursiveUpdate {} (lib.optionalAttrs config.services.syncthing.enable {
          "/syncthing/" = {
            proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.syncthing}/";
            extraConfig = ''
              proxy_read_timeout 600s;
              proxy_send_timeout 600s;
            '';
          };
        } // lib.optionalAttrs config.services.adguardhome.enable {
          "/adguard/" = {
            proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.adguard}/";
          };
        } // lib.optionalAttrs config.services.deluge.enable {
          "/deluge/" = {
            proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.deglue}/";
            extraConfig = ''
              proxy_set_header X-Deluge-Base "/deluge/";
              add_header X-Frame-Options SAMEORIGIN;
            '';
          };
        } // lib.optionalAttrs config.services.sonarr.enable {
            "/sonarr" = {
              proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.sonarr}";
              extraConfig = ''
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
              '';
            };
          } // lib.optionalAttrs config.services.radarr.enable {
              "/radarr" = {
                  proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.radarr}";
                  extraConfig = ''
                    proxy_http_version 1.1;
                    proxy_set_header Upgrade $http_upgrade;
                    proxy_set_header Connection "upgrade";
                    proxy_redirect off;
               '';
              };
         } // lib.optionalAttrs config.services.prowlarr.enable {
           "/prowlarr" = {
               proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.prowlarr}";
               extraConfig = ''
                 proxy_http_version 1.1;
                 proxy_set_header Upgrade $http_upgrade;
                 proxy_set_header Connection "upgrade";
                 proxy_redirect off;
                 '';
           };
         } // lib.optionalAttrs config.services.jackett.enable {
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


        );
      };

      # Conditionally add virtual hosts based on enabled services
      "deluge.${config.userDefinedGlobalVariables.hostConfigurationName}" = lib.optionalAttrs config.services.deluge.enable {
        listen = [
          {
            addr = "${allInterfaces}";
            port = httpPort;
          }
        ];
        locations."/" = {
          proxyPass = "${localHost}:${builtins.toString config.userDefinedGlobalVariables.servicePort.deglue}/";
          extraConfig = ''
            proxy_set_header X-Deluge-Base "/";
            add_header X-Frame-Options SAMEORIGIN;
          '';
        };
      };

      "adguard.${config.userDefinedGlobalVariables.hostConfigurationName}" = lib.optionalAttrs config.services.adguardhome.enable {
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

      "syncthing.${config.userDefinedGlobalVariables.hostConfigurationName}" = lib.optionalAttrs config.services.syncthing.enable {
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
      "sonarr.${config.userDefinedGlobalVariables.hostConfigurationName}" = lib.optionalAttrs config.services.sonarr.enable {
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

       "radarr.${config.userDefinedGlobalVariables.hostConfigurationName}" = lib.optionalAttrs config.services.radarr.enable {
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

      "prowlarr.${config.userDefinedGlobalVariables.hostConfigurationName}" = lib.optionalAttrs config.services.prowlarr.enable {
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

      "jackett.${config.userDefinedGlobalVariables.hostConfigurationName}" = lib.optionalAttrs config.services.jackett.enable {
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






    };
  };

  networking.firewall.allowedTCPPorts = [
    httpPort
    httpsPort
  ];
}

