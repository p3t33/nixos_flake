{ config, lib, ... }:

let
  serviceName = "prowlarr";
in
{
    config = lib.mkIf config.services.${serviceName}.enable {
    sops.secrets."${serviceName}/env" = {};

    services.${serviceName} = {
      openFirewall = true;
      settings = {
        server = {
          port = 9696;
          urlbase = "/${serviceName}";
        };

        postgres = {
          host   = "127.0.0.1";
          port   = 5432;
          user   = "prowlarr";
          maindb = "prowlarr_main";
          logdb  = "prowlarr_log";
        };
      };

      environmentFiles = [
        config.sops.secrets."${serviceName}/env".path
      ];

    };
  };
}
