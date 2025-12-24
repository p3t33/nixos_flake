{ config, lib, pkgs, ... }:

let
  gatus = "gatus";
  prowlarr = "prowlarr";
  sonarr = "sonarr";
  radarr = "radarr";
  n8n = "n8n";
in
{
  config = lib.mkIf config.services.postgresql.enable {


    # sops.secrets =
    #   {}
    #   // lib.mkIf config.services.gatus.enable {
    #      "postgresql/gatus" = {
    #       owner = config.systemd.services.postgresql.serviceConfig.User;
    #       restartUnits = [ config.systemd.services.postgresql.name ];
    #     };
    #   }
    #   // lib.mkIf config.services.prowlarr.enable {
    #      "postgresql/prowlarr" = {
    #        owner = config.systemd.services.postgresql.serviceConfig.User;
    #        restartUnits = [ config.systemd.services.postgresql.name ];
    #      };
    #   };

    #     sops.secrets."postgresql/gatus" = {
    #   owner = config.systemd.services.postgresql.serviceConfig.User;
    #   restartUnits = [ config.systemd.services.postgresql.name ];
    # };
    #
    # sops.secrets."postgresql/prowlarr" = {
    #   owner = config.systemd.services.postgresql.serviceConfig.User;
    #   restartUnits = [ config.systemd.services.postgresql.name ];
    # };


    sops.secrets."postgresql/gatus" = lib.mkIf config.services.gatus.enable {
      owner = config.systemd.services.postgresql.serviceConfig.User;
      restartUnits = [ config.systemd.services.postgresql.name ];
    };

    sops.secrets."postgresql/prowlarr" = lib.mkIf config.services.prowlarr.enable {
      owner = config.systemd.services.postgresql.serviceConfig.User;
      restartUnits = [ config.systemd.services.postgresql.name ];
    };

    sops.secrets."postgresql/sonarr" = lib.mkIf config.services.sonarr.enable {
      owner = config.systemd.services.postgresql.serviceConfig.User;
      restartUnits = [ config.systemd.services.postgresql.name ];
    };

    sops.secrets."postgresql/radarr" = lib.mkIf config.services.radarr.enable {
      owner = config.systemd.services.postgresql.serviceConfig.User;
      restartUnits = [ config.systemd.services.postgresql.name ];
    };

    services.postgresql = {
      settings = {
        port = 5432;
      };

      # Removing users that were created using this confiugration will not remove them on postgresql side,
      # meanin you will need to do this manually.
      ensureUsers =
        [ ]
        ++ lib.optionals config.services.gatus.enable [
          {
            name = "${gatus}";
            ensureDBOwnership = true; # owns DB named gatus.
          }
        ]
        ++ lib.optionals config.services.${prowlarr}.enable [
          {
            name = "${config.custom.services.${prowlarr}.postgresUserName}";
          }
        ]
        ++ lib.optionals config.services.${sonarr}.enable [
          {
            name = "${config.custom.services.${sonarr}.postgresUserName}";
          }
        ]
        ++ lib.optionals config.services.${radarr}.enable [
          {
            name = "${config.custom.services.${radarr}.postgresUserName}";
          }
        ]
        ++ lib.optionals config.services.${n8n}.enable [
          {
            name = "${config.services.n8n.environment.DB_POSTGRESDB_USER}";
            ensureDBOwnership = true; # <-- add this
          }
        ];

      # Removing databases that were created using this confiugration will not remove them on postgresql side,
      # meanin you will need to do this manually.
      ensureDatabases =
        []
        ++ lib.optionals config.services.gatus.enable [
          "${gatus}"
        ]
        ++ lib.optionals config.services.${prowlarr}.enable [
          "${config.custom.services.${prowlarr}.mainDataBase}"
          "${config.custom.services.${prowlarr}.logDataBase}"
        ]
        ++ lib.optionals config.services.${sonarr}.enable [
          "${config.custom.services.${sonarr}.mainDataBase}"
          "${config.custom.services.${sonarr}.logDataBase}"
        ]
        ++ lib.optionals config.services.${radarr}.enable [
          "${config.custom.services.${radarr}.mainDataBase}"
          "${config.custom.services.${radarr}.logDataBase}"
        ]
        ++ lib.optionals config.services.n8n.enable [
          "${config.services.n8n.environment.DB_POSTGRESDB_DATABASE}"
        ];

      authentication = ''
        local all all                peer
        host  all all 127.0.0.1/32   md5
        host  all all ::1/128        md5
      '';
    };

    # As there is no way to declare passwords for non peer users, this oneshot
    # service is a workaround.
    systemd.services.postgresql-set-passwords-for-non-peer-access = {
     description = "Set PostgreSQL password for non peer access with SOPS secret";
     after = [ config.systemd.services.postgresql.name] ;
     requires = [ config.systemd.services.postgresql.name ];
     wantedBy = [ config.systemd.targets.multi-user.name ];
     serviceConfig = {
       Type = "oneshot";
       User = config.systemd.services.postgresql.serviceConfig.User;
       ExecStart = pkgs.writeShellScript "set-non-peer-passwords" ''
         ${pkgs.postgresql}/bin/psql -U postgres -tA <<'EOF'
           ${lib.optionalString config.services.gatus.enable ''
           DO $$
           DECLARE password TEXT;
           BEGIN
             password := trim(both from replace(pg_read_file('${config.sops.secrets."postgresql/gatus".path}'), E'\n', '''));
             EXECUTE format('ALTER ROLE ${gatus} WITH PASSWORD '''%s''';', password);
           END $$;
           ''}

           ${lib.optionalString config.services.prowlarr.enable ''
           DO $$
           DECLARE password TEXT;
           BEGIN
             password := trim(both from replace(pg_read_file('${config.sops.secrets."postgresql/prowlarr".path}'), E'\n', '''));
             EXECUTE format('ALTER ROLE ${config.custom.services.${prowlarr}.postgresUserName} WITH PASSWORD '''%s''';', password);
           END $$;

           -- Make prowlarr the owner of both databases (idempotent)
           ALTER DATABASE "${config.custom.services.${prowlarr}.mainDataBase}" OWNER TO ${config.custom.services.${prowlarr}.postgresUserName};
           ALTER DATABASE "${config.custom.services.${prowlarr}.logDataBase}"  OWNER TO ${config.custom.services.${prowlarr}.postgresUserName};
           ''}

           ${lib.optionalString config.services.sonarr.enable ''
           DO $$
           DECLARE password TEXT;
           BEGIN
             password := trim(both from replace(pg_read_file('${config.sops.secrets."postgresql/sonarr".path}'), E'\n', '''));
             EXECUTE format('ALTER ROLE ${config.custom.services.sonarr.postgresUserName} WITH PASSWORD '''%s''';', password);
           END $$;

           -- Make sonarr the owner of both databases (idempotent)
           ALTER DATABASE "${config.custom.services.sonarr.mainDataBase}" OWNER TO ${config.custom.services.sonarr.postgresUserName};
           ALTER DATABASE "${config.custom.services.sonarr.logDataBase}"  OWNER TO ${config.custom.services.sonarr.postgresUserName};
           ''}

           ${lib.optionalString config.services.radarr.enable ''
           DO $$
           DECLARE password TEXT;
           BEGIN
             password := trim(both from replace(pg_read_file('${config.sops.secrets."postgresql/radarr".path}'), E'\n', '''));
             EXECUTE format('ALTER ROLE ${config.custom.services.radarr.postgresUserName} WITH PASSWORD '''%s''';', password);
           END $$;

           -- Make radarr the owner of both databases (idempotent)
           ALTER DATABASE "${config.custom.services.radarr.mainDataBase}" OWNER TO ${config.custom.services.radarr.postgresUserName};
           ALTER DATABASE "${config.custom.services.radarr.logDataBase}"  OWNER TO ${config.custom.services.radarr.postgresUserName};
           ''}
         EOF
       '';
       # This is less PostgreSQL-idiomatic way to do it but it was tested to work.
       # ExecStart = pkgs.writeShellScript "set-non-peer-passwords" ''
       #   pw=$(tr -d '\n' < ${config.sops.secrets."postgresql/gatus".path})
       #   echo "Setting password for ${gatus}"
       #   ${pkgs.postgresql}/bin/psql -U postgres -c "DO \$\$ DECLARE password TEXT; BEGIN password := '$pw'; EXECUTE format('ALTER ROLE ${gatus} WITH PASSWORD %L;', password); END \$\$;"
     };
  };
};
}
