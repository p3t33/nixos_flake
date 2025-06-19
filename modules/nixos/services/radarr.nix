{ config, lib, ... }:

let
  serviceName = "radarr";
in
{

  config = lib.mkIf config.services.${serviceName}.enable {
    sops.secrets."${serviceName}/apiKey" = {};

    services.${serviceName} = {
      openFirewall = true; # Opens Sonarr's port on the firewall (default 8989)
      user = "${serviceName}";
      group = "${config.customGlobal.mediaGroup}";
      settings = {
          server = {
            port = 7878;
            urlbase = "/${serviceName}";
        };
      };

      environmentFiles = [
        config.sops.secrets."${serviceName}/apiKey".path
      ];
    };
  };
}
