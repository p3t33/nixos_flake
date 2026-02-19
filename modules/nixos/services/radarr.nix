{ config, lib, ... }:

let
  serviceName = "radarr";
in
{
  options.custom.services.${serviceName} = {
    user = lib.mkOption {
      type = lib.types.str;
      default = "${serviceName}";
      description = "PostgreSQL role/user for ${serviceName}.";
    };

    mainDataBase = lib.mkOption {
      type = lib.types.str;
      default = "${serviceName}_main";
      description = "Main DB name for ${serviceName}. Null -> <user>_main.";
    };

    logDataBase = lib.mkOption {
      type = lib.types.str;
      default = "${serviceName}_log";
      description = "Log DB name for ${serviceName}. Null -> <user>_log.";
    };

    postgresUserName = lib.mkOption {
      type = lib.types.str;
      default = "${serviceName}";
      description = "PostgreSQL username for ${serviceName}.";
    };
  };

  config = lib.mkIf config.services.${serviceName}.enable {

    sops.secrets."${serviceName}/env" = {
      restartUnits = [ config.systemd.services.${serviceName}.name ];
    };

    systemd.tmpfiles.rules = [
      "d ${config.custom.shared.pathToMediaDirectory}/movies 0770 ${config.services.radarr.user} ${config.custom.shared.mediaGroup} -"
    ];

    services.${serviceName} = {
      openFirewall = true; # Opens Sonarr's port on the firewall (default 8989)
      user = "${serviceName}";
      group = "${config.custom.shared.mediaGroup}";
      settings = {
        server = {
          port = 7878;
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

      environmentFiles = [
        config.sops.secrets."${serviceName}/env".path
      ];
    };
  };
}
