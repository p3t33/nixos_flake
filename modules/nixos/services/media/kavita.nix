{ config, lib, pkgs, ... }:
let
  cfg = config.services.kavita;
  mediaDirectory = config.custom.shared.pathToMediaDirectory;
  kavitaDirectory = "${mediaDirectory}/kavita";
  mangaDirectory = "${kavitaDirectory}/manga";
  comicsDirectory = "${kavitaDirectory}/comics";
  setupMarker = "${cfg.dataDir}/.nixos-admin-created";
  apiUrl = "http://${config.custom.shared.localHostIPv4}:${toString cfg.settings.Port}";
in
{
  config = lib.mkIf cfg.enable {
    sops.secrets."kavita/tokenKey".restartUnits = [ config.systemd.services.kavita.name ];
    sops.secrets."kavita/username".restartUnits = [ config.systemd.services.kavita-setup.name ];
    sops.secrets."kavita/password".restartUnits = [ config.systemd.services.kavita-setup.name ];

    systemd.tmpfiles.rules = [
      "d ${kavitaDirectory} 2770 ${cfg.user} ${config.custom.shared.mediaGroup} -"
      "d ${mangaDirectory} 2770 ${cfg.user} ${config.custom.shared.mediaGroup} -"
      "d ${comicsDirectory} 2770 ${cfg.user} ${config.custom.shared.mediaGroup} -"
    ];

    users.users.${cfg.user}.extraGroups = [ config.custom.shared.mediaGroup ];

    services.kavita = {
      tokenKeyFile = config.sops.secrets."kavita/tokenKey".path;
      settings = {
        IpAddresses = config.custom.shared.localHostIPv4;
        Port = 5000;
      };
    };

    systemd.services.kavita.unitConfig.RequiresMountsFor = [ kavitaDirectory ];

    systemd.services.kavita-setup = {
      description = "Create the initial Kavita administrator";
      wantedBy = [ "multi-user.target" ];
      after = [
        config.systemd.services.kavita.name
        "network.target"
      ];
      requires = [ config.systemd.services.kavita.name ];
      unitConfig = {
        ConditionPathExists = "!${setupMarker}";
        RequiresMountsFor = [ kavitaDirectory ];
      };
      path = [ pkgs.curl pkgs.jq ];
      script = ''
        set -euo pipefail
        umask 0077

        ready=false
        for _ in $(seq 1 12); do
          if curl --disable --noproxy '*' --fail --silent --output /dev/null \
            --connect-timeout 2 --max-time 4 "${apiUrl}/"; then
            ready=true
            break
          fi
          sleep 1
        done

        if [ "$ready" != true ]; then
          echo "Kavita did not become ready within 60 seconds" >&2
          exit 1
        fi

        response="$RUNTIME_DIRECTORY/account-response.json"
        trap 'rm -f "$response"' EXIT
        registration_status=$(
          jq -n \
            --rawfile username "$CREDENTIALS_DIRECTORY/username" \
            --rawfile password "$CREDENTIALS_DIRECTORY/password" \
            '{ Username: ($username | rtrimstr("\n")), Password: ($password | rtrimstr("\n")) }' \
          | curl --disable --noproxy '*' --silent --show-error --output "$response" --write-out '%{http_code}' \
              --connect-timeout 5 --max-time 15 --header 'Content-Type: application/json' --data-binary @- \
              "${apiUrl}/api/account/register"
        )

        if [ "$registration_status" = 200 ]; then
          touch "${setupMarker}"
          exit 0
        fi

        if [ "$registration_status" != 400 ]; then
          echo "Kavita initial-admin registration failed with HTTP $registration_status" >&2
          exit 1
        fi

        login_status=$(
          jq -n \
            --rawfile username "$CREDENTIALS_DIRECTORY/username" \
            --rawfile password "$CREDENTIALS_DIRECTORY/password" \
            '{ Username: ($username | rtrimstr("\n")), Password: ($password | rtrimstr("\n")) }' \
          | curl --disable --noproxy '*' --silent --show-error --output "$response" --write-out '%{http_code}' \
              --connect-timeout 5 --max-time 15 --header 'Content-Type: application/json' --data-binary @- \
              "${apiUrl}/api/account/login"
        )

        if [ "$login_status" != 200 ]; then
          echo "Kavita did not accept registration, and the configured credentials cannot log in" >&2
          exit 1
        fi

        if ! jq --exit-status '(.roles // .Roles // []) | index("Admin") != null' "$response" >/dev/null; then
          echo "The configured Kavita account is not an administrator" >&2
          exit 1
        fi

        touch "${setupMarker}"
      '';
      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        Group = cfg.user;
        UMask = "0077";
        RuntimeDirectory = "kavita-setup";
        RuntimeDirectoryMode = "0700";
        TimeoutStartSec = "120s";
        LoadCredential = [
          "username:${config.sops.secrets."kavita/username".path}"
          "password:${config.sops.secrets."kavita/password".path}"
        ];
      };
    };
  };
}
