{ config, ... }:

let
  serviceName = "prowlarr";
in
{
  sops.secrets."${serviceName}/apiKey" = {};

  services.${serviceName} = {
    enable = true;

    openFirewall = true;
    settings = {
      server = {
        port = 9696;
        urlbase = "/${serviceName}";
      };
    };

    environmentFiles = [
      config.sops.secrets."${serviceName}/apiKey".path
    ];

  };
}
