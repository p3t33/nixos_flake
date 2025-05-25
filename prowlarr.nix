{ config, ... }:

let
  serviceName = "prowlarr";
in
{
  sops.secrets."${serviceName}/apiKey" = {};

  # Enable the Sonarr service(as of now there is no config for default sonarr port)
  services.${serviceName} = {
    enable = true;
    openFirewall = true; # Opens Sonarr's port on the firewall (default 8989)
    user = "${serviceName}";
    group = "${config.userDefinedGlobalVariables.mediaGroup}";
    settings = {
      server = {
          port = config.userDefinedGlobalVariables.servicePort.${serviceName};
          urlbase = "/${serviceName}";
      };
    };

    environmentFiles = [
      config.sops.secrets."${serviceName}/apiKey".path
    ];
  };

}
