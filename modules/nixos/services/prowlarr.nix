{ config, lib, ... }:

let
  serviceName = "prowlarr";
in
{
   options.custom.services.${serviceName} = {
    user = lib.mkOption {
      type = lib.types.str;
      default = "${serviceName}";
      description = "PostgreSQL role/user for Prowlarr.";
    };

    mainDataBase = lib.mkOption {
      type = lib.types.str;
      default = "${serviceName}_main";
      description = "Main DB name for Prowlarr. Null -> <user>_main.";
    };

    logDataBase = lib.mkOption {
      type = lib.types.str;
      default = "${serviceName}_log";
      description = "Log DB name for Prowlarr. Null -> <user>_log.";
    };

    postgresUserName = lib.mkOption {
      type = lib.types.str;
      default = "${serviceName}";
      description = "Log DB name for Prowlarr. Null -> <user>_log.";
    };
  };

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
          host   = "${config.custom.shared.localHostIPv4}";
          port   = config.services.postgresql.settings.port;
          user   = "${config.custom.services.${serviceName}.postgresUserName}";
          maindb = "${config.custom.services.${serviceName}.mainDataBase}";
          logdb  = "${config.custom.services.${serviceName}.logDataBase}";
        };
      };

      # Variables inside the file will take presedends over the settings = {...}
      environmentFiles = [
        config.sops.secrets."${serviceName}/env".path
      ];

    };
  };
}
