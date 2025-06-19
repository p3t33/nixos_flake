{ config, lib, ... }:

let
  serviceName = "prowlarr";
in
{
    config = lib.mkIf config.services.${serviceName}.enable {
    sops.secrets."${serviceName}/apiKey" = {};

    services.${serviceName} = {
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
  };
}
