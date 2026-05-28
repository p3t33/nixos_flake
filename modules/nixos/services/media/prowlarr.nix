{
  config,
  lib,
  pkgs,
  ...
}:

let
  arrHelpers = import ./lib/arr-api-helpers.nix { inherit lib; };
  serviceName = "prowlarr";
  prowlarrBaseUrl = "http://${config.custom.shared.localHostIPv4}:${toString config.services.prowlarr.settings.server.port}${config.services.prowlarr.settings.server.urlbase}";
  prowlarrEnvCredential = "prowlarr-env";
  sonarrEnvCredential = "sonarr-env";
  radarrEnvCredential = "radarr-env";

  waitForProwlarrApi = pkgs.writeShellScript "wait-for-prowlarr-api" ''
    CURL_CONFIG=$(mktemp)
    chmod 600 "$CURL_CONFIG"
    trap 'rm -f "$CURL_CONFIG"' EXIT
    printf 'header = "X-Api-Key: %s"\n' "$PROWLARR__AUTH__APIKEY" > "$CURL_CONFIG"

    for attempt in $(seq 1 90); do
      if curl -fsS -K "$CURL_CONFIG" \
        "${prowlarrBaseUrl}/api/v1/system/status" >/dev/null 2>&1; then
        exit 0
      fi
      sleep 1
    done
    echo "Prowlarr API did not become ready" >&2
    exit 1
  '';

  waitForProwlarrApiPre = "${pkgs.curl}/bin/curl --retry 30 --retry-delay 2 --retry-connrefused -so /dev/null ${prowlarrBaseUrl}/api/v1/system/status";

  mkProwlarrApplication = appName: implementationName: baseUrl: apiKeyFileVar: ''
    upsert_application ${lib.escapeShellArg appName} ${lib.escapeShellArg implementationName} ${lib.escapeShellArg baseUrl} "${"$"}${apiKeyFileVar}"
  '';
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
      description = "PostgreSQL username for Prowlarr.";
    };
  };

  config = lib.mkIf config.services.${serviceName}.enable {

    sops.secrets."${serviceName}/env" = {
      restartUnits = [
        config.systemd.services.${serviceName}.name
      ]
      ++ lib.optional (
        config.services.sonarr.enable || config.services.radarr.enable
      ) config.systemd.services.prowlarr-applications.name;
    };

    sops.secrets."sonarr/env".restartUnits = lib.mkIf config.services.sonarr.enable (
      lib.mkAfter [
        config.systemd.services.prowlarr-applications.name
      ]
    );

    sops.secrets."radarr/env".restartUnits = lib.mkIf config.services.radarr.enable (
      lib.mkAfter [
        config.systemd.services.prowlarr-applications.name
      ]
    );

    services.${serviceName} = {
      openFirewall = true;
      settings = {
        server = {
          port = 9696;
          urlbase = "/${serviceName}";
        };

        postgres = {
          host = "${config.custom.shared.localHostIPv4}";
          port = config.services.postgresql.settings.port;
          user = "${config.custom.services.${serviceName}.postgresUserName}";
          maindb = "${config.custom.services.${serviceName}.mainDataBase}";
          logdb = "${config.custom.services.${serviceName}.logDataBase}";
        };
      };

      environmentFiles = [
        config.sops.secrets."${serviceName}/env".path
      ];

    };

    systemd.services.${serviceName} = {
      path = [ pkgs.curl ];
      serviceConfig.ExecStartPost = "${waitForProwlarrApi}";
    };

    systemd.services.prowlarr-applications =
      lib.mkIf (config.services.sonarr.enable || config.services.radarr.enable)
        {
          description = "Configure Prowlarr applications";
          after = [
            config.systemd.services.prowlarr.name
          ]
          ++ lib.optional config.services.sonarr.enable config.systemd.services.sonarr.name
          ++ lib.optional config.services.radarr.enable config.systemd.services.radarr.name;
          wantedBy = [ "multi-user.target" ];

          path = with pkgs; [
            coreutils
            curl
            jq
          ];

          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStartPre = waitForProwlarrApiPre;
            LoadCredential = [
              "${prowlarrEnvCredential}:${config.sops.secrets."${serviceName}/env".path}"
            ]
            ++ lib.optional config.services.sonarr.enable "${sonarrEnvCredential}:${config.sops.secrets."sonarr/env".path}"
            ++ lib.optional config.services.radarr.enable "${radarrEnvCredential}:${config.sops.secrets."radarr/env".path}";
          };

          script = ''
            set -euo pipefail

            . "$CREDENTIALS_DIRECTORY/${prowlarrEnvCredential}"
            ${lib.optionalString config.services.sonarr.enable ''. "$CREDENTIALS_DIRECTORY/${sonarrEnvCredential}"''}
            ${lib.optionalString config.services.radarr.enable ''. "$CREDENTIALS_DIRECTORY/${radarrEnvCredential}"''}

            : "''${PROWLARR__AUTH__APIKEY:?PROWLARR__AUTH__APIKEY is required}"
            ${lib.optionalString config.services.sonarr.enable '': "''${SONARR__AUTH__APIKEY:?SONARR__AUTH__APIKEY is required}"''}
            ${lib.optionalString config.services.radarr.enable '': "''${RADARR__AUTH__APIKEY:?RADARR__AUTH__APIKEY is required}"''}

            BASE_URL=${lib.escapeShellArg "${prowlarrBaseUrl}/api/v1"}
            PROWLARR_URL=${lib.escapeShellArg prowlarrBaseUrl}

            ${arrHelpers.common}

            new_curl_config PROWLARR_CURL_CONFIG "$PROWLARR__AUTH__APIKEY"
            ${lib.optionalString config.services.sonarr.enable ''
              new_curl_config SONARR_CURL_CONFIG "$SONARR__AUTH__APIKEY"
              new_secret_file SONARR_API_KEY_FILE "$SONARR__AUTH__APIKEY"
            ''}
            ${lib.optionalString config.services.radarr.enable ''
              new_curl_config RADARR_CURL_CONFIG "$RADARR__AUTH__APIKEY"
              new_secret_file RADARR_API_KEY_FILE "$RADARR__AUTH__APIKEY"
            ''}

            ${arrHelpers.mkCurlWrapper "prowlarr" "PROWLARR_CURL_CONFIG"}

            SCHEMAS=$(curl_prowlarr "$BASE_URL/applications/schema")
            APPLICATIONS=$(curl_prowlarr "$BASE_URL/applications")

            upsert_application() {
              local app_name="$1"
              local implementation_name="$2"
              local app_base_url="$3"
              local app_api_key_file="$4"
              local existing_app app_id payload schema

              echo "Configuring Prowlarr application: $app_name"

              existing_app=$(echo "$APPLICATIONS" | jq -c --arg name "$app_name" '.[] | select(.name == $name)' | head -n1)

              build_payload() {
                jq \
                  --arg name "$app_name" \
                  --arg baseUrl "$app_base_url" \
                  --arg prowlarrUrl "$PROWLARR_URL" \
                  --rawfile apiKey "$app_api_key_file" \
                  '
                    .name = $name
                    | .syncLevel = "addOnly"
                    | .fields |= map(
                        if .name == "baseUrl" then .value = $baseUrl
                        elif .name == "prowlarrUrl" then .value = $prowlarrUrl
                        elif .name == "apiKey" then .value = $apiKey
                        else .
                        end
                      )
                  '
              }

              if [ -n "$existing_app" ]; then
                app_id=$(echo "$existing_app" | jq -r '.id')
                payload=$(echo "$existing_app" | build_payload)

                printf '%s' "$payload" | curl_prowlarr "$BASE_URL/applications/$app_id" \
                  -X PUT \
                  -H "Content-Type: application/json" \
                  --data-binary @- \
                  >/dev/null

                echo "Updated Prowlarr application: $app_name"
              else
                schema=$(echo "$SCHEMAS" | jq -c --arg implementationName "$implementation_name" '.[] | select(.implementationName == $implementationName)' | head -n1)

                if [ -z "$schema" ]; then
                  echo "No Prowlarr application schema found for $implementation_name" >&2
                  exit 1
                fi

                payload=$(echo "$schema" | build_payload)

                printf '%s' "$payload" | curl_prowlarr "$BASE_URL/applications" \
                  -X POST \
                  -H "Content-Type: application/json" \
                  --data-binary @- \
                  >/dev/null

                echo "Created Prowlarr application: $app_name"
              fi
            }

            ${lib.optionalString config.services.sonarr.enable (
              mkProwlarrApplication "Sonarr" "Sonarr"
                "http://${config.custom.shared.localHostIPv4}:${toString config.services.sonarr.settings.server.port}${config.services.sonarr.settings.server.urlbase}"
                "SONARR_API_KEY_FILE"
            )}
            ${lib.optionalString config.services.radarr.enable (
              mkProwlarrApplication "Radarr" "Radarr"
                "http://${config.custom.shared.localHostIPv4}:${toString config.services.radarr.settings.server.port}${config.services.radarr.settings.server.urlbase}"
                "RADARR_API_KEY_FILE"
            )}
          '';
        };
  };
}
