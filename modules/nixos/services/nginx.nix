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
        });
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
    };
  };

  networking.firewall.allowedTCPPorts = [
    httpPort
    httpsPort
  ];
}

