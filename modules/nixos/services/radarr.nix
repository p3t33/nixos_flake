{ config, ... }:
let
  serviceName = "radarr";
in
{
  sops.secrets."${serviceName}/apiKey" = {};

  services.${serviceName} = {
    enable = true;
    openFirewall = true; # Opens Sonarr's port on the firewall (default 8989)
    user = "${serviceName}";
    group = "${config.customGlobalOptions.mediaGroup}";
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
}
