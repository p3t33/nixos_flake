{ config, lib, pkgs, ... }:

let
  gatus = "gatus";
in
{
  config = lib.mkIf config.services.postgresql.enable {
    sops.secrets."postgresql/gatus" = {
      owner = config.systemd.services.postgresql.serviceConfig.User;
      restartUnits = [ config.systemd.services.postgresql.name ];
    };

    services.postgresql = {
      settings = {
        port = 5432;
      };

      # Removing users that were created using this confiugration will not remove them on postgresql side,
      # meanin you will need to do this manually.
      ensureUsers = [
        {
          name = "${gatus}";
          ensureDBOwnership = true;
        }
      ];

      # Removing databases that were created using this confiugration will not remove them on postgresql side,
      # meanin you will need to do this manually.
      ensureDatabases = [
        "${gatus}"
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
           DO $$
           DECLARE password TEXT;
           BEGIN
             password := trim(both from replace(pg_read_file('${config.sops.secrets."postgresql/gatus".path}'), E'\n', '''));
             EXECUTE format('ALTER ROLE ${gatus} WITH PASSWORD '''%s''';', password);
           END $$;
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
