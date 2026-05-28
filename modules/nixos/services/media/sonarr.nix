{
  config,
  lib,
  pkgs,
  ...
}:

let
  arrHelpers = import ./lib/arr-api-helpers.nix { inherit lib; };
  serviceName = "sonarr";
  sonarrBaseUrl = "http://${config.custom.shared.localHostIPv4}:${toString config.services.sonarr.settings.server.port}${config.services.sonarr.settings.server.urlbase}";
  tvRootFolder = "${config.custom.shared.pathToMediaDirectory}/tv";
  sabnzbdBaseUrl = "http://${config.custom.shared.localHostIPv4}:${toString config.custom.services.sabnzbd.httpPort}/sabnzbd";
  categories = config.custom.media.downloadCategories;
  sonarrEnvCredential = "sonarr-env";
  sabnzbdApiKeyCredential = "sabnzbd-api-key";
  delugePasswordCredential = "deluge-password";
  qbittorrentUsernameCredential = "qbittorrent-username";
  qbittorrentPasswordCredential = "qbittorrent-password";

  waitForSonarrApi = pkgs.writeShellScript "wait-for-sonarr-api" ''
    CURL_CONFIG=$(mktemp)
    chmod 600 "$CURL_CONFIG"
    trap 'rm -f "$CURL_CONFIG"' EXIT
    printf 'header = "X-Api-Key: %s"\n' "$SONARR__AUTH__APIKEY" > "$CURL_CONFIG"

    for attempt in $(seq 1 90); do
      if curl -fsS -K "$CURL_CONFIG" \
        "${sonarrBaseUrl}/api/v3/system/status" >/dev/null 2>&1; then
        exit 0
      fi
      sleep 1
    done
    echo "Sonarr API did not become ready" >&2
    exit 1
  '';

  waitForSonarrApiPre = "${pkgs.curl}/bin/curl --retry 30 --retry-delay 2 --retry-connrefused -so /dev/null ${sonarrBaseUrl}/api/v3/system/status";
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
      restartUnits = [
        config.systemd.services.${serviceName}.name
        config.systemd.services.sonarr-rootfolders.name
        config.systemd.services.sonarr-delayprofiles.name
      ]
      ++ lib.optional (config.services.sabnzbd.enable || config.services.deluge.enable || config.services.qbittorrent.enable)
        config.systemd.services.sonarr-downloadclients.name;
    };

    sops.secrets."sabnzbd/api_key".restartUnits = lib.mkIf config.services.sabnzbd.enable (
      lib.mkAfter [
        config.systemd.services.sonarr-downloadclients.name
      ]
    );

    sops.secrets."deluge/web_password".restartUnits = lib.mkIf config.services.deluge.enable (
      lib.mkAfter [
        config.systemd.services.sonarr-downloadclients.name
      ]
    );

    sops.secrets."qbittorrent/webui_username".restartUnits = lib.mkIf config.services.qbittorrent.enable (
      lib.mkAfter [
        config.systemd.services.sonarr-downloadclients.name
      ]
    );

    sops.secrets."qbittorrent/webui_password".restartUnits = lib.mkIf config.services.qbittorrent.enable (
      lib.mkAfter [
        config.systemd.services.sonarr-downloadclients.name
      ]
    );

    systemd.tmpfiles.rules = [
      "d ${tvRootFolder} 2770 ${config.services.sonarr.user} ${config.custom.shared.mediaGroup} -"
    ];

    # Enable the Sonarr service(as of now there is no config for default sonarr port)
    services.${serviceName} = {
      openFirewall = true;
      user = "${serviceName}";
      group = "${config.custom.shared.mediaGroup}";
      settings = {
        server = {
          port = 8989;
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
      serviceConfig.ExecStartPost = "${waitForSonarrApi}";
    };

    systemd.services.sonarr-downloadclients = lib.mkIf
      (config.services.sabnzbd.enable || config.services.deluge.enable || config.services.qbittorrent.enable)
    {
      description = "Configure Sonarr download clients";
      after = [ config.systemd.services.${serviceName}.name ]
        ++ lib.optional config.services.sabnzbd.enable config.systemd.services.sabnzbd.name
        ++ lib.optional config.services.deluge.enable config.systemd.services.delugeweb.name
        ++ lib.optional config.services.qbittorrent.enable config.systemd.services.qbittorrent.name;
      wantedBy = [ "multi-user.target" ];

      path = with pkgs; [
        coreutils
        curl
        jq
      ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStartPre = waitForSonarrApiPre;
        LoadCredential = [
          "${sonarrEnvCredential}:${config.sops.secrets."${serviceName}/env".path}"
        ]
        ++ lib.optional config.services.sabnzbd.enable
          "${sabnzbdApiKeyCredential}:${config.sops.secrets."sabnzbd/api_key".path}"
        ++ lib.optional config.services.deluge.enable
          "${delugePasswordCredential}:${config.sops.secrets."deluge/web_password".path}"
        ++ lib.optionals config.services.qbittorrent.enable [
          "${qbittorrentUsernameCredential}:${config.sops.secrets."qbittorrent/webui_username".path}"
          "${qbittorrentPasswordCredential}:${config.sops.secrets."qbittorrent/webui_password".path}"
        ];
      };

      script = ''
        set -euo pipefail

        . "$CREDENTIALS_DIRECTORY/${sonarrEnvCredential}"
        : "''${SONARR__AUTH__APIKEY:?SONARR__AUTH__APIKEY is required}"

        BASE_URL=${lib.escapeShellArg "${sonarrBaseUrl}/api/v3"}
        HOST=${lib.escapeShellArg config.custom.shared.localHostIPv4}
        CATEGORY=${lib.escapeShellArg categories.tv}

        ${arrHelpers.common}
        new_curl_config SONARR_CURL_CONFIG "$SONARR__AUTH__APIKEY"
        ${arrHelpers.mkCurlWrapper "sonarr" "SONARR_CURL_CONFIG"}

        SCHEMAS=$(curl_sonarr "$BASE_URL/downloadclient/schema")
        DOWNLOAD_CLIENTS=$(curl_sonarr "$BASE_URL/downloadclient")

        ${arrHelpers.upsertClient}

        ${lib.optionalString config.services.sabnzbd.enable ''
          SABNZBD_URL=${lib.escapeShellArg sabnzbdBaseUrl}
          SABNZBD_PORT=${lib.escapeShellArg (toString config.custom.services.sabnzbd.httpPort)}
          SABNZBD_API_KEY_CREDENTIAL="$CREDENTIALS_DIRECTORY/${sabnzbdApiKeyCredential}"
          new_temp_file SABNZBD_API_KEY_FILE
          tr -d '\n' < "$SABNZBD_API_KEY_CREDENTIAL" > "$SABNZBD_API_KEY_FILE"
          SABNZBD_API_KEY=$(cat "$SABNZBD_API_KEY_FILE")
          new_temp_file SABNZBD_VERSION_CURL_CONFIG
          printf 'url = "%s/api?mode=version&apikey=%s&output=json"\n' "$SABNZBD_URL" "$SABNZBD_API_KEY" > "$SABNZBD_VERSION_CURL_CONFIG"

          build_sabnzbd_payload() {
            jq \
              --arg name "SABnzbd" \
              --arg host "$HOST" \
              --argjson port "$SABNZBD_PORT" \
              --arg urlBase "/sabnzbd" \
              --rawfile apiKey "$SABNZBD_API_KEY_FILE" \
              --arg tvCategory "$CATEGORY" \
              '
                .name = $name
                | .enable = true
                | .priority = 1
                | .fields |= map(
                    if .name == "host" then .value = $host
                    elif .name == "port" then .value = $port
                    elif .name == "urlBase" then .value = $urlBase
                    elif .name == "apiKey" then .value = $apiKey
                    elif .name == "tvCategory" then .value = $tvCategory
                    else .
                    end
                  )
              '
          }

          echo "Waiting for SABnzbd API..."
          if curl -fsS -K "$SABNZBD_VERSION_CURL_CONFIG" --retry 10 --retry-delay 2 --retry-connrefused >/dev/null 2>&1; then
            upsert_client "SABnzbd" "SABnzbd" build_sabnzbd_payload curl_sonarr || echo "WARNING: Failed to configure SABnzbd client, skipping"
          else
            echo "WARNING: SABnzbd not available, skipping"
          fi
        ''}

        ${lib.optionalString config.services.qbittorrent.enable ''
          QBITTORRENT_URL=${lib.escapeShellArg "http://${config.custom.shared.localHostIPv4}:${toString config.services.qbittorrent.webuiPort}"}
          QBITTORRENT_PORT=${lib.escapeShellArg (toString config.services.qbittorrent.webuiPort)}
          QBITTORRENT_USERNAME_CREDENTIAL="$CREDENTIALS_DIRECTORY/${qbittorrentUsernameCredential}"
          QBITTORRENT_PASSWORD_CREDENTIAL="$CREDENTIALS_DIRECTORY/${qbittorrentPasswordCredential}"
          new_temp_file QBITTORRENT_USERNAME_FILE
          new_temp_file QBITTORRENT_PASSWORD_FILE
          tr -d '\n' < "$QBITTORRENT_USERNAME_CREDENTIAL" > "$QBITTORRENT_USERNAME_FILE"
          tr -d '\n' < "$QBITTORRENT_PASSWORD_CREDENTIAL" > "$QBITTORRENT_PASSWORD_FILE"

          build_qbittorrent_payload() {
            jq \
              --arg name "qBittorrent" \
              --arg host "$HOST" \
              --argjson port "$QBITTORRENT_PORT" \
              --rawfile username "$QBITTORRENT_USERNAME_FILE" \
              --rawfile password "$QBITTORRENT_PASSWORD_FILE" \
              --arg category "$CATEGORY" \
              '
                .name = $name
                | .enable = true
                | .priority = 2
                | .fields |= map(
                    if .name == "host" then .value = $host
                    elif .name == "port" then .value = $port
                    elif .name == "username" then .value = $username
                    elif .name == "password" then .value = $password
                    elif .name == "category" then .value = $category
                    elif .name == "tvCategory" then .value = $category
                    else .
                    end
                  )
              '
          }

          echo "Waiting for qBittorrent Web UI..."
          if curl -fsS "$QBITTORRENT_URL/" --retry 10 --retry-delay 2 --retry-connrefused >/dev/null 2>&1; then
            upsert_client "qBittorrent" "qBittorrent" build_qbittorrent_payload curl_sonarr || echo "WARNING: Failed to configure qBittorrent client, skipping"
          else
            echo "WARNING: qBittorrent not available, skipping"
          fi
        ''}

        ${lib.optionalString config.services.deluge.enable ''
          DELUGE_URL=${lib.escapeShellArg "http://${config.custom.shared.localHostIPv4}:${toString config.services.deluge.web.port}"}
          DELUGE_PORT=${lib.escapeShellArg (toString config.services.deluge.web.port)}
          DELUGE_PASSWORD_CREDENTIAL="$CREDENTIALS_DIRECTORY/${delugePasswordCredential}"
          new_temp_file DELUGE_PASSWORD_FILE
          tr -d '\n' < "$DELUGE_PASSWORD_CREDENTIAL" > "$DELUGE_PASSWORD_FILE"

          build_deluge_payload() {
            jq \
              --arg name "Deluge" \
              --arg host "$HOST" \
              --argjson port "$DELUGE_PORT" \
              --rawfile password "$DELUGE_PASSWORD_FILE" \
              --arg category "$CATEGORY" \
              '
                .name = $name
                | .enable = true
                | .priority = 3
                | .fields |= map(
                    if .name == "host" then .value = $host
                    elif .name == "port" then .value = $port
                    elif .name == "password" then .value = $password
                    elif .name == "category" then .value = $category
                    elif .name == "tvCategory" then .value = $category
                    else .
                    end
                  )
              '
          }

          echo "Waiting for Deluge Web UI..."
          if curl -fsS "$DELUGE_URL/" --retry 10 --retry-delay 2 --retry-connrefused >/dev/null 2>&1; then
            upsert_client "Deluge" "Deluge" build_deluge_payload curl_sonarr || echo "WARNING: Failed to configure Deluge client, skipping"
          else
            echo "WARNING: Deluge not available, skipping"
          fi
        ''}

        echo "Sonarr download clients configured"
      '';
    };

    systemd.services.sonarr-delayprofiles = {
      description = "Configure Sonarr delay profiles";
      after = [ config.systemd.services.${serviceName}.name ];
      wantedBy = [ "multi-user.target" ];

      path = with pkgs; [
        coreutils
        curl
        jq
      ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStartPre = waitForSonarrApiPre;
        LoadCredential = [
          "${sonarrEnvCredential}:${config.sops.secrets."${serviceName}/env".path}"
        ];
      };

      script = ''
        set -euo pipefail

        . "$CREDENTIALS_DIRECTORY/${sonarrEnvCredential}"
        : "''${SONARR__AUTH__APIKEY:?SONARR__AUTH__APIKEY is required}"

        BASE_URL=${lib.escapeShellArg "${sonarrBaseUrl}/api/v3"}
        PREFERRED_PROTOCOL="usenet"
        USENET_DELAY=0
        TORRENT_DELAY=1440

        ${arrHelpers.common}
        new_curl_config SONARR_CURL_CONFIG "$SONARR__AUTH__APIKEY"
        ${arrHelpers.mkCurlWrapper "sonarr" "SONARR_CURL_CONFIG"}

        DELAY_PROFILES=$(curl_sonarr "$BASE_URL/delayprofile")
        existing_profile=$(echo "$DELAY_PROFILES" | jq -c 'map(select(((.tags // []) | length) == 0)) | sort_by(.order // 2147483647) | .[0] // empty')

        build_payload() {
          jq \
            --arg preferredProtocol "$PREFERRED_PROTOCOL" \
            --argjson usenetDelay "$USENET_DELAY" \
            --argjson torrentDelay "$TORRENT_DELAY" \
            '
              .enableUsenet = true
              | .enableTorrent = true
              | .preferredProtocol = $preferredProtocol
              | .usenetDelay = $usenetDelay
              | .torrentDelay = $torrentDelay
              | .bypassIfHighestQuality = false
              | .bypassIfAboveCustomFormatScore = false
              | .minimumCustomFormatScore = 0
              | .order = 2147483647
              | .tags = []
            '
        }

        if [ -n "$existing_profile" ]; then
          profile_id=$(echo "$existing_profile" | jq -r '.id')
          payload=$(echo "$existing_profile" | build_payload)

          printf '%s' "$payload" | curl_sonarr "$BASE_URL/delayprofile/$profile_id" \
            -X PUT \
            -H "Content-Type: application/json" \
            --data-binary @- \
            >/dev/null

          echo "Updated Sonarr global delay profile"
        else
          payload=$(jq -n '{}' | build_payload)

          printf '%s' "$payload" | curl_sonarr "$BASE_URL/delayprofile" \
            -X POST \
            -H "Content-Type: application/json" \
            --data-binary @- \
            >/dev/null

          echo "Created Sonarr global delay profile"
        fi
      '';
    };

    systemd.services.sonarr-rootfolders = {
      description = "Configure Sonarr root folders";
      after = [
        config.systemd.services.${serviceName}.name
        "systemd-tmpfiles-setup.service"
      ];
      wantedBy = [ "multi-user.target" ];

      path = with pkgs; [
        coreutils
        curl
        jq
      ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStartPre = waitForSonarrApiPre;
        LoadCredential = [
          "${sonarrEnvCredential}:${config.sops.secrets."${serviceName}/env".path}"
        ];
      };

      script = ''
        set -euo pipefail

        . "$CREDENTIALS_DIRECTORY/${sonarrEnvCredential}"
        : "''${SONARR__AUTH__APIKEY:?SONARR__AUTH__APIKEY is required}"

        BASE_URL=${lib.escapeShellArg "${sonarrBaseUrl}/api/v3"}
        ROOT_FOLDER=${lib.escapeShellArg tvRootFolder}

        ${arrHelpers.common}
        new_curl_config SONARR_CURL_CONFIG "$SONARR__AUTH__APIKEY"
        ${arrHelpers.mkCurlWrapper "sonarr" "SONARR_CURL_CONFIG"}

        ROOT_FOLDERS=$(curl_sonarr "$BASE_URL/rootfolder")

        if echo "$ROOT_FOLDERS" | jq -e --arg path "$ROOT_FOLDER" '.[] | select(.path == $path)' >/dev/null; then
          echo "Sonarr root folder already exists: $ROOT_FOLDER"
        else
          echo "Creating Sonarr root folder: $ROOT_FOLDER"
          jq -n --arg path "$ROOT_FOLDER" '{ path: $path }' | curl_sonarr "$BASE_URL/rootfolder" \
            -X POST \
            -H "Content-Type: application/json" \
            --data-binary @- \
            >/dev/null
          echo "Created Sonarr root folder: $ROOT_FOLDER"
        fi
      '';
    };
  };
}
