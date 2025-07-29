{ config, lib, ... }:

let
  serviceName = "sonarr";
in
{

  config = lib.mkIf config.services.${serviceName}.enable {
    sops.secrets."${serviceName}/apiKey" = {};

    # Enable the Sonarr service(as of now there is no config for default sonarr port)
    services.${serviceName} = {
      openFirewall = true; # Opens Sonarr's port on the firewall (default 8989)
      user = "${serviceName}";
      group = "${config.customGlobal.mediaGroup}";
      settings = {
          server = {
            port = 8989;
            urlbase = "/${serviceName}";
        };
      };

      environmentFiles = [
        config.sops.secrets."${serviceName}/apiKey".path
      ];
    };
  };
}
