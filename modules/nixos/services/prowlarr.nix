{ config, lib, ... }:

let
  serviceName = "prowlarr";
in
{
  config = lib.mkIf config.services.${serviceName}.enable {

    sops.secrets."${serviceName}/env" = {
      restartUnits = [ config.systemd.services.${serviceName}.name ];
    };

    services.${serviceName} = {
      openFirewall = true;
      settings = {
        server = {
          port = 9696;
          urlbase = "/${serviceName}";
        };

        postgres = {
          host   = "${config.customGlobal.localHostIPv4}";
          port   = config.services.postgresql.settings.port;
          user   = "prowlarr";
          maindb = "prowlarr_main";
          logdb  = "prowlarr_log";
        };
      };

      # Variables inside the file will take presedends over the settings = {...}
      environmentFiles = [
        config.sops.secrets."${serviceName}/env".path
      ];

    };
  };
}
