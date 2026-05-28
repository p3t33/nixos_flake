{
  config,
  lib,
  pkgs,
  ...
}:

let
  serviceName = "radarr";
  radarrBaseUrl = "http://${config.custom.shared.localHostIPv4}:${toString config.services.radarr.settings.server.port}${config.services.radarr.settings.server.urlbase}";
  moviesRootFolder = "${config.custom.shared.pathToMediaDirectory}/movies";
  sabnzbdBaseUrl = "http://${config.custom.shared.localHostIPv4}:${toString config.custom.services.sabnzbd.httpPort}/sabnzbd";
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
        config.systemd.services.radarr-rootfolders.name
        config.systemd.services.radarr-delayprofiles.name
      ]
      ++ lib.optional config.services.sabnzbd.enable config.systemd.services.radarr-downloadclients.name
      ++ lib.optional config.services.deluge.enable config.systemd.services.radarr-deluge-downloadclient.name
      ++ lib.optional config.services.qbittorrent.enable config.systemd.services.radarr-qbittorrent-downloadclient.name;
    };

    sops.secrets."sabnzbd/api_key".restartUnits = lib.mkIf config.services.sabnzbd.enable (
      lib.mkAfter [
        config.systemd.services.radarr-downloadclients.name
      ]
    );

    sops.secrets."deluge/web_password".restartUnits = lib.mkIf config.services.deluge.enable (
      lib.mkAfter [
        config.systemd.services.radarr-deluge-downloadclient.name
      ]
    );

    sops.secrets."qbittorrent/webui_username".restartUnits = lib.mkIf config.services.qbittorrent.enable (
      lib.mkAfter [
        config.systemd.services.radarr-qbittorrent-downloadclient.name
      ]
    );

    sops.secrets."qbittorrent/webui_password".restartUnits = lib.mkIf config.services.qbittorrent.enable (
      lib.mkAfter [
        config.systemd.services.radarr-qbittorrent-downloadclient.name
      ]
    );

    systemd.tmpfiles.rules = [
      "d ${moviesRootFolder} 2770 ${config.services.radarr.user} ${config.custom.shared.mediaGroup} -"
    ];

    services.${serviceName} = {
      openFirewall = true; # Opens Radarr's port on the firewall (default 7878)
      user = "${serviceName}";
      group = "${config.custom.shared.mediaGroup}";
      settings = {
        server = {
          port = 7878;
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

    systemd.services.radarr-downloadclients = lib.mkIf config.services.sabnzbd.enable {
      description = "Configure Radarr download clients";
      after = [
        config.systemd.services.${serviceName}.name
        config.systemd.services.sabnzbd.name
      ];
      requires = [
        config.systemd.services.${serviceName}.name
        config.systemd.services.sabnzbd.name
      ];
      wantedBy = [ config.systemd.services.${serviceName}.name ];

      path = with pkgs; [
        coreutils
        curl
        jq
      ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        EnvironmentFile = config.sops.secrets."${serviceName}/env".path;
      };

      script = ''
        set -euo pipefail

        : "''${RADARR__AUTH__APIKEY:?RADARR__AUTH__APIKEY is required}"

        BASE_URL=${lib.escapeShellArg "${radarrBaseUrl}/api/v3"}
        SABNZBD_URL=${lib.escapeShellArg sabnzbdBaseUrl}
        SABNZBD_PORT=${lib.escapeShellArg (toString config.custom.services.sabnzbd.httpPort)}
        SABNZBD_API_KEY_SECRET=${lib.escapeShellArg config.sops.secrets."sabnzbd/api_key".path}
        DOWNLOAD_CLIENT_NAME="SABnzbd"
        MOVIE_CATEGORY="movies"

        TMP_FILES=()
        cleanup() {
          rm -f "''${TMP_FILES[@]}"
        }
        trap cleanup EXIT

        new_temp_file() {
          local result_var="$1"
          local tmp_file

          tmp_file=$(mktemp)
          chmod 600 "$tmp_file"
          TMP_FILES+=("$tmp_file")
          printf -v "$result_var" '%s' "$tmp_file"
        }

        new_curl_config() {
          local result_var="$1"
          local api_key="$2"
          local file

          new_temp_file file
          printf 'header = "X-Api-Key: %s"\n' "$api_key" > "$file"
          printf -v "$result_var" '%s' "$file"
        }

        new_curl_config RADARR_CURL_CONFIG "$RADARR__AUTH__APIKEY"
        new_temp_file SABNZBD_API_KEY_FILE
        tr -d '\n' < "$SABNZBD_API_KEY_SECRET" > "$SABNZBD_API_KEY_FILE"
        SABNZBD_API_KEY=$(cat "$SABNZBD_API_KEY_FILE")
        new_temp_file SABNZBD_VERSION_CURL_CONFIG
        printf 'url = "%s/api?mode=version&apikey=%s&output=json"\n' "$SABNZBD_URL" "$SABNZBD_API_KEY" > "$SABNZBD_VERSION_CURL_CONFIG"

        curl_radarr() {
          local url="$1"
          shift

          curl -fsS -K "$RADARR_CURL_CONFIG" "$@" "$url"
        }

        echo "Waiting for Radarr API..."
        for attempt in $(seq 1 60); do
          if curl_radarr "$BASE_URL/system/status" >/dev/null 2>&1; then
            break
          fi

          if [ "$attempt" -eq 60 ]; then
            echo "Timed out waiting for Radarr API" >&2
            exit 1
          fi

          sleep 2
        done

        echo "Waiting for SABnzbd API..."
        for attempt in $(seq 1 60); do
          if curl -fsS -K "$SABNZBD_VERSION_CURL_CONFIG" >/dev/null 2>&1; then
            break
          fi

          if [ "$attempt" -eq 60 ]; then
            echo "Timed out waiting for SABnzbd API" >&2
            exit 1
          fi

          sleep 2
        done

        SCHEMAS=$(curl_radarr "$BASE_URL/downloadclient/schema")
        DOWNLOAD_CLIENTS=$(curl_radarr "$BASE_URL/downloadclient")

        build_payload() {
          jq \
            --arg name "$DOWNLOAD_CLIENT_NAME" \
            --arg host ${lib.escapeShellArg config.custom.shared.localHostIPv4} \
            --argjson port "$SABNZBD_PORT" \
            --arg urlBase "/sabnzbd" \
            --rawfile apiKey "$SABNZBD_API_KEY_FILE" \
            --arg movieCategory "$MOVIE_CATEGORY" \
            '
              .name = $name
              | .enable = true
              | .priority = 1
              | .fields |= map(
                  if .name == "host" then .value = $host
                  elif .name == "port" then .value = $port
                  elif .name == "urlBase" then .value = $urlBase
                  elif .name == "apiKey" then .value = $apiKey
                  elif .name == "movieCategory" then .value = $movieCategory
                  else .
                  end
                )
            '
        }

        existing_client=$(echo "$DOWNLOAD_CLIENTS" | jq -c --arg name "$DOWNLOAD_CLIENT_NAME" '.[] | select(.name == $name)' | head -n1)

        if [ -n "$existing_client" ] && [ "$(echo "$existing_client" | jq -r '.implementationName')" != "SABnzbd" ]; then
          echo "Download client named $DOWNLOAD_CLIENT_NAME exists but is not SABnzbd" >&2
          exit 1
        fi

        if [ -n "$existing_client" ]; then
          client_id=$(echo "$existing_client" | jq -r '.id')
          payload=$(echo "$existing_client" | build_payload)

          printf '%s' "$payload" | curl_radarr "$BASE_URL/downloadclient/$client_id" \
            -X PUT \
            -H "Content-Type: application/json" \
            --data-binary @- \
            >/dev/null

          echo "Updated Radarr download client: $DOWNLOAD_CLIENT_NAME"
        else
          schema=$(echo "$SCHEMAS" | jq -c '.[] | select(.implementationName == "SABnzbd")' | head -n1)

          if [ -z "$schema" ]; then
            echo "No Radarr download client schema found for SABnzbd" >&2
            exit 1
          fi

          payload=$(echo "$schema" | build_payload)

          printf '%s' "$payload" | curl_radarr "$BASE_URL/downloadclient" \
            -X POST \
            -H "Content-Type: application/json" \
            --data-binary @- \
            >/dev/null

          echo "Created Radarr download client: $DOWNLOAD_CLIENT_NAME"
        fi
      '';
    };

    systemd.services.radarr-deluge-downloadclient = lib.mkIf config.services.deluge.enable {
      description = "Configure Radarr Deluge download client";
      after = [
        config.systemd.services.${serviceName}.name
        config.systemd.services.delugeweb.name
      ];
      requires = [
        config.systemd.services.${serviceName}.name
        config.systemd.services.delugeweb.name
      ];
      wantedBy = [ config.systemd.services.${serviceName}.name ];

      path = with pkgs; [
        coreutils
        curl
        jq
      ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        EnvironmentFile = config.sops.secrets."${serviceName}/env".path;
      };

      script = ''
        set -euo pipefail

        : "''${RADARR__AUTH__APIKEY:?RADARR__AUTH__APIKEY is required}"

        BASE_URL=${lib.escapeShellArg "${radarrBaseUrl}/api/v3"}
        DELUGE_URL=${lib.escapeShellArg "http://${config.custom.shared.localHostIPv4}:${toString config.services.deluge.web.port}"}
        DELUGE_PORT=${lib.escapeShellArg (toString config.services.deluge.web.port)}
        DELUGE_PASSWORD_SECRET=${lib.escapeShellArg config.sops.secrets."deluge/web_password".path}
        DOWNLOAD_CLIENT_NAME="Deluge"
        MOVIE_CATEGORY="movies"

        TMP_FILES=()
        cleanup() {
          rm -f "''${TMP_FILES[@]}"
        }
        trap cleanup EXIT

        new_temp_file() {
          local result_var="$1"
          local tmp_file

          tmp_file=$(mktemp)
          chmod 600 "$tmp_file"
          TMP_FILES+=("$tmp_file")
          printf -v "$result_var" '%s' "$tmp_file"
        }

        new_curl_config() {
          local result_var="$1"
          local api_key="$2"
          local file

          new_temp_file file
          printf 'header = "X-Api-Key: %s"\n' "$api_key" > "$file"
          printf -v "$result_var" '%s' "$file"
        }

        new_curl_config RADARR_CURL_CONFIG "$RADARR__AUTH__APIKEY"
        new_temp_file DELUGE_PASSWORD_FILE
        tr -d '\n' < "$DELUGE_PASSWORD_SECRET" > "$DELUGE_PASSWORD_FILE"

        curl_radarr() {
          local url="$1"
          shift

          curl -fsS -K "$RADARR_CURL_CONFIG" "$@" "$url"
        }

        echo "Waiting for Radarr API..."
        for attempt in $(seq 1 60); do
          if curl_radarr "$BASE_URL/system/status" >/dev/null 2>&1; then
            break
          fi

          if [ "$attempt" -eq 60 ]; then
            echo "Timed out waiting for Radarr API" >&2
            exit 1
          fi

          sleep 2
        done

        echo "Waiting for Deluge Web UI..."
        for attempt in $(seq 1 60); do
          if curl -fsS "$DELUGE_URL/" >/dev/null 2>&1; then
            break
          fi

          if [ "$attempt" -eq 60 ]; then
            echo "Timed out waiting for Deluge Web UI" >&2
            exit 1
          fi

          sleep 2
        done

        SCHEMAS=$(curl_radarr "$BASE_URL/downloadclient/schema")
        DOWNLOAD_CLIENTS=$(curl_radarr "$BASE_URL/downloadclient")

        build_payload() {
          jq \
            --arg name "$DOWNLOAD_CLIENT_NAME" \
            --arg host ${lib.escapeShellArg config.custom.shared.localHostIPv4} \
            --argjson port "$DELUGE_PORT" \
            --rawfile password "$DELUGE_PASSWORD_FILE" \
            --arg category "$MOVIE_CATEGORY" \
            '
              .name = $name
              | .enable = true
              | .priority = 3
              | .fields |= map(
                  if .name == "host" then .value = $host
                  elif .name == "port" then .value = $port
                  elif .name == "password" then .value = $password
                  elif .name == "category" then .value = $category
                  elif .name == "movieCategory" then .value = $category
                  else .
                  end
                )
            '
        }

        existing_client=$(echo "$DOWNLOAD_CLIENTS" | jq -c --arg name "$DOWNLOAD_CLIENT_NAME" '.[] | select(.name == $name)' | head -n1)

        if [ -n "$existing_client" ] && [ "$(echo "$existing_client" | jq -r '.implementationName')" != "Deluge" ]; then
          echo "Download client named $DOWNLOAD_CLIENT_NAME exists but is not Deluge" >&2
          exit 1
        fi

        if [ -n "$existing_client" ]; then
          client_id=$(echo "$existing_client" | jq -r '.id')
          payload=$(echo "$existing_client" | build_payload)

          printf '%s' "$payload" | curl_radarr "$BASE_URL/downloadclient/$client_id" \
            -X PUT \
            -H "Content-Type: application/json" \
            --data-binary @- \
            >/dev/null

          echo "Updated Radarr download client: $DOWNLOAD_CLIENT_NAME"
        else
          schema=$(echo "$SCHEMAS" | jq -c '.[] | select(.implementationName == "Deluge")' | head -n1)

          if [ -z "$schema" ]; then
            echo "No Radarr download client schema found for Deluge" >&2
            exit 1
          fi

          payload=$(echo "$schema" | build_payload)

          printf '%s' "$payload" | curl_radarr "$BASE_URL/downloadclient" \
            -X POST \
            -H "Content-Type: application/json" \
            --data-binary @- \
            >/dev/null

          echo "Created Radarr download client: $DOWNLOAD_CLIENT_NAME"
        fi
      '';
    };

    systemd.services.radarr-qbittorrent-downloadclient = lib.mkIf config.services.qbittorrent.enable {
      description = "Configure Radarr qBittorrent download client";
      after = [
        config.systemd.services.${serviceName}.name
        config.systemd.services.qbittorrent.name
      ];
      requires = [
        config.systemd.services.${serviceName}.name
        config.systemd.services.qbittorrent.name
      ];
      wantedBy = [ config.systemd.services.${serviceName}.name ];

      path = with pkgs; [
        coreutils
        curl
        jq
      ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        EnvironmentFile = config.sops.secrets."${serviceName}/env".path;
      };

      script = ''
        set -euo pipefail

        : "''${RADARR__AUTH__APIKEY:?RADARR__AUTH__APIKEY is required}"

        BASE_URL=${lib.escapeShellArg "${radarrBaseUrl}/api/v3"}
        QBITTORRENT_URL=${lib.escapeShellArg "http://${config.custom.shared.localHostIPv4}:${toString config.services.qbittorrent.webuiPort}"}
        QBITTORRENT_PORT=${lib.escapeShellArg (toString config.services.qbittorrent.webuiPort)}
        QBITTORRENT_USERNAME_SECRET=${lib.escapeShellArg config.sops.secrets."qbittorrent/webui_username".path}
        QBITTORRENT_PASSWORD_SECRET=${lib.escapeShellArg config.sops.secrets."qbittorrent/webui_password".path}
        DOWNLOAD_CLIENT_NAME="qBittorrent"
        MOVIE_CATEGORY="movies"

        TMP_FILES=()
        cleanup() {
          rm -f "''${TMP_FILES[@]}"
        }
        trap cleanup EXIT

        new_temp_file() {
          local result_var="$1"
          local tmp_file

          tmp_file=$(mktemp)
          chmod 600 "$tmp_file"
          TMP_FILES+=("$tmp_file")
          printf -v "$result_var" '%s' "$tmp_file"
        }

        new_curl_config() {
          local result_var="$1"
          local api_key="$2"
          local file

          new_temp_file file
          printf 'header = "X-Api-Key: %s"\n' "$api_key" > "$file"
          printf -v "$result_var" '%s' "$file"
        }

        new_curl_config RADARR_CURL_CONFIG "$RADARR__AUTH__APIKEY"
        new_temp_file QBITTORRENT_USERNAME_FILE
        new_temp_file QBITTORRENT_PASSWORD_FILE
        tr -d '\n' < "$QBITTORRENT_USERNAME_SECRET" > "$QBITTORRENT_USERNAME_FILE"
        tr -d '\n' < "$QBITTORRENT_PASSWORD_SECRET" > "$QBITTORRENT_PASSWORD_FILE"

        curl_radarr() {
          local url="$1"
          shift

          curl -fsS -K "$RADARR_CURL_CONFIG" "$@" "$url"
        }

        echo "Waiting for Radarr API..."
        for attempt in $(seq 1 60); do
          if curl_radarr "$BASE_URL/system/status" >/dev/null 2>&1; then
            break
          fi

          if [ "$attempt" -eq 60 ]; then
            echo "Timed out waiting for Radarr API" >&2
            exit 1
          fi

          sleep 2
        done

        echo "Waiting for qBittorrent Web UI..."
        for attempt in $(seq 1 60); do
          if curl -fsS "$QBITTORRENT_URL/" >/dev/null 2>&1; then
            break
          fi

          if [ "$attempt" -eq 60 ]; then
            echo "Timed out waiting for qBittorrent Web UI" >&2
            exit 1
          fi

          sleep 2
        done

        SCHEMAS=$(curl_radarr "$BASE_URL/downloadclient/schema")
        DOWNLOAD_CLIENTS=$(curl_radarr "$BASE_URL/downloadclient")

        build_payload() {
          jq \
            --arg name "$DOWNLOAD_CLIENT_NAME" \
            --arg host ${lib.escapeShellArg config.custom.shared.localHostIPv4} \
            --argjson port "$QBITTORRENT_PORT" \
            --rawfile username "$QBITTORRENT_USERNAME_FILE" \
            --rawfile password "$QBITTORRENT_PASSWORD_FILE" \
            --arg category "$MOVIE_CATEGORY" \
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
                  elif .name == "movieCategory" then .value = $category
                  else .
                  end
                )
            '
        }

        existing_client=$(echo "$DOWNLOAD_CLIENTS" | jq -c --arg name "$DOWNLOAD_CLIENT_NAME" '.[] | select(.name == $name)' | head -n1)

        if [ -n "$existing_client" ] && [ "$(echo "$existing_client" | jq -r '.implementationName')" != "qBittorrent" ]; then
          echo "Download client named $DOWNLOAD_CLIENT_NAME exists but is not qBittorrent" >&2
          exit 1
        fi

        if [ -n "$existing_client" ]; then
          client_id=$(echo "$existing_client" | jq -r '.id')
          payload=$(echo "$existing_client" | build_payload)

          printf '%s' "$payload" | curl_radarr "$BASE_URL/downloadclient/$client_id" \
            -X PUT \
            -H "Content-Type: application/json" \
            --data-binary @- \
            >/dev/null

          echo "Updated Radarr download client: $DOWNLOAD_CLIENT_NAME"
        else
          schema=$(echo "$SCHEMAS" | jq -c '.[] | select(.implementationName == "qBittorrent")' | head -n1)

          if [ -z "$schema" ]; then
            echo "No Radarr download client schema found for qBittorrent" >&2
            exit 1
          fi

          payload=$(echo "$schema" | build_payload)

          printf '%s' "$payload" | curl_radarr "$BASE_URL/downloadclient" \
            -X POST \
            -H "Content-Type: application/json" \
            --data-binary @- \
            >/dev/null

          echo "Created Radarr download client: $DOWNLOAD_CLIENT_NAME"
        fi
      '';
    };

    systemd.services.radarr-delayprofiles = {
      description = "Configure Radarr delay profiles";
      after = [ config.systemd.services.${serviceName}.name ];
      requires = [ config.systemd.services.${serviceName}.name ];
      wantedBy = [ config.systemd.services.${serviceName}.name ];

      path = with pkgs; [
        coreutils
        curl
        jq
      ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        EnvironmentFile = config.sops.secrets."${serviceName}/env".path;
      };

      script = ''
        set -euo pipefail

        : "''${RADARR__AUTH__APIKEY:?RADARR__AUTH__APIKEY is required}"

        BASE_URL=${lib.escapeShellArg "${radarrBaseUrl}/api/v3"}
        PREFERRED_PROTOCOL="usenet"
        USENET_DELAY=0
        TORRENT_DELAY=1440

        CURL_CONFIG=$(mktemp)
        cleanup() {
          rm -f "$CURL_CONFIG"
        }
        trap cleanup EXIT

        chmod 600 "$CURL_CONFIG"
        # Keep the API key out of child process argv by passing it through curl config.
        printf 'header = "X-Api-Key: %s"\n' "$RADARR__AUTH__APIKEY" > "$CURL_CONFIG"

        curl_radarr() {
          local url="$1"
          shift

          curl -fsS -K "$CURL_CONFIG" "$@" "$url"
        }

        echo "Waiting for Radarr API..."
        for attempt in $(seq 1 60); do
          if curl_radarr "$BASE_URL/system/status" >/dev/null 2>&1; then
            break
          fi

          if [ "$attempt" -eq 60 ]; then
            echo "Timed out waiting for Radarr API" >&2
            exit 1
          fi

          sleep 2
        done

        DELAY_PROFILES=$(curl_radarr "$BASE_URL/delayprofile")
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

          printf '%s' "$payload" | curl_radarr "$BASE_URL/delayprofile/$profile_id" \
            -X PUT \
            -H "Content-Type: application/json" \
            --data-binary @- \
            >/dev/null

          echo "Updated Radarr global delay profile"
        else
          payload=$(jq -n '{}' | build_payload)

          printf '%s' "$payload" | curl_radarr "$BASE_URL/delayprofile" \
            -X POST \
            -H "Content-Type: application/json" \
            --data-binary @- \
            >/dev/null

          echo "Created Radarr global delay profile"
        fi
      '';
    };

    systemd.services.radarr-rootfolders = {
      description = "Configure Radarr root folders";
      after = [
        config.systemd.services.${serviceName}.name
        "systemd-tmpfiles-setup.service"
      ];
      requires = [
        config.systemd.services.${serviceName}.name
        "systemd-tmpfiles-setup.service"
      ];
      wantedBy = [ config.systemd.services.${serviceName}.name ];

      path = with pkgs; [
        coreutils
        curl
        jq
      ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        EnvironmentFile = config.sops.secrets."${serviceName}/env".path;
      };

      script = ''
        set -euo pipefail

        : "''${RADARR__AUTH__APIKEY:?RADARR__AUTH__APIKEY is required}"

        BASE_URL=${lib.escapeShellArg "${radarrBaseUrl}/api/v3"}
        ROOT_FOLDER=${lib.escapeShellArg moviesRootFolder}

        CURL_CONFIG=$(mktemp)
        cleanup() {
          rm -f "$CURL_CONFIG"
        }
        trap cleanup EXIT

        chmod 600 "$CURL_CONFIG"
        printf 'header = "X-Api-Key: %s"\n' "$RADARR__AUTH__APIKEY" > "$CURL_CONFIG"

        curl_radarr() {
          local url="$1"
          shift

          curl -fsS -K "$CURL_CONFIG" "$@" "$url"
        }

        echo "Waiting for Radarr API..."
        for attempt in $(seq 1 60); do
          if curl_radarr "$BASE_URL/system/status" >/dev/null 2>&1; then
            break
          fi

          if [ "$attempt" -eq 60 ]; then
            echo "Timed out waiting for Radarr API" >&2
            exit 1
          fi

          sleep 2
        done

        ROOT_FOLDERS=$(curl_radarr "$BASE_URL/rootfolder")

        if echo "$ROOT_FOLDERS" | jq -e --arg path "$ROOT_FOLDER" '.[] | select(.path == $path)' >/dev/null; then
          echo "Radarr root folder already exists: $ROOT_FOLDER"
        else
          echo "Creating Radarr root folder: $ROOT_FOLDER"
          jq -n --arg path "$ROOT_FOLDER" '{ path: $path }' | curl_radarr "$BASE_URL/rootfolder" \
            -X POST \
            -H "Content-Type: application/json" \
            --data-binary @- \
            >/dev/null
          echo "Created Radarr root folder: $ROOT_FOLDER"
        fi
      '';
    };
  };
}
