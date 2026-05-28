{
  config,
  lib,
  pkgs,
  ...
}:

let
  serviceName = "prowlarr";
  prowlarrBaseUrl = "http://${config.custom.shared.localHostIPv4}:${toString config.services.prowlarr.settings.server.port}${config.services.prowlarr.settings.server.urlbase}";

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

      # Variables inside the file will take presedends over the settings = {...}
      environmentFiles = [
        config.sops.secrets."${serviceName}/env".path
      ];

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
          requires = [
            config.systemd.services.prowlarr.name
          ]
          ++ lib.optional config.services.sonarr.enable config.systemd.services.sonarr.name
          ++ lib.optional config.services.radarr.enable config.systemd.services.radarr.name;
          wantedBy = [ config.systemd.services.${serviceName}.name ];

          path = with pkgs; [
            coreutils
            curl
            jq
          ];

          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            EnvironmentFile = [
              config.sops.secrets."${serviceName}/env".path
            ]
            ++ lib.optional config.services.sonarr.enable config.sops.secrets."sonarr/env".path
            ++ lib.optional config.services.radarr.enable config.sops.secrets."radarr/env".path;
          };

          script = ''
            set -euo pipefail

            : "''${PROWLARR__AUTH__APIKEY:?PROWLARR__AUTH__APIKEY is required}"
            ${lib.optionalString config.services.sonarr.enable '': "''${SONARR__AUTH__APIKEY:?SONARR__AUTH__APIKEY is required}"''}
            ${lib.optionalString config.services.radarr.enable '': "''${RADARR__AUTH__APIKEY:?RADARR__AUTH__APIKEY is required}"''}

            BASE_URL=${lib.escapeShellArg "${prowlarrBaseUrl}/api/v1"}
            PROWLARR_URL=${lib.escapeShellArg prowlarrBaseUrl}

            TMP_FILES=()
            cleanup() {
              rm -f "''${TMP_FILES[@]}"
            }
            trap cleanup EXIT

            new_secret_file() {
              local result_var="$1"
              local value="$2"
              local file

              file=$(mktemp)
              chmod 600 "$file"
              printf '%s' "$value" > "$file"
              TMP_FILES+=("$file")
              printf -v "$result_var" '%s' "$file"
            }

            new_curl_config() {
              local result_var="$1"
              local api_key="$2"
              local file

              file=$(mktemp)
              chmod 600 "$file"
              printf 'header = "X-Api-Key: %s"\n' "$api_key" > "$file"
              TMP_FILES+=("$file")
              printf -v "$result_var" '%s' "$file"
            }

            new_curl_config PROWLARR_CURL_CONFIG "$PROWLARR__AUTH__APIKEY"
            ${lib.optionalString config.services.sonarr.enable ''
              new_curl_config SONARR_CURL_CONFIG "$SONARR__AUTH__APIKEY"
              new_secret_file SONARR_API_KEY_FILE "$SONARR__AUTH__APIKEY"
            ''}
            ${lib.optionalString config.services.radarr.enable ''
              new_curl_config RADARR_CURL_CONFIG "$RADARR__AUTH__APIKEY"
              new_secret_file RADARR_API_KEY_FILE "$RADARR__AUTH__APIKEY"
            ''}

            curl_prowlarr() {
              local url="$1"
              shift

              curl -fsS -K "$PROWLARR_CURL_CONFIG" "$@" "$url"
            }

            wait_for_api() {
              local name="$1"
              local status_url="$2"
              local curl_config="$3"

              echo "Waiting for $name API..."
              for attempt in $(seq 1 60); do
                if curl -fsS -K "$curl_config" "$status_url" >/dev/null 2>&1; then
                  return 0
                fi

                if [ "$attempt" -eq 60 ]; then
                  echo "Timed out waiting for $name API" >&2
                  exit 1
                fi

                sleep 2
              done
            }

            wait_for_api "Prowlarr" "$BASE_URL/system/status" "$PROWLARR_CURL_CONFIG"
            ${lib.optionalString config.services.sonarr.enable ''wait_for_api "Sonarr" ${lib.escapeShellArg "http://${config.custom.shared.localHostIPv4}:${toString config.services.sonarr.settings.server.port}${config.services.sonarr.settings.server.urlbase}/api/v3/system/status"} "$SONARR_CURL_CONFIG"''}
            ${lib.optionalString config.services.radarr.enable ''wait_for_api "Radarr" ${lib.escapeShellArg "http://${config.custom.shared.localHostIPv4}:${toString config.services.radarr.settings.server.port}${config.services.radarr.settings.server.urlbase}/api/v3/system/status"} "$RADARR_CURL_CONFIG"''}

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
