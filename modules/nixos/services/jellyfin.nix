{
  pkgs,
  config,
  lib,
  ...
}:

let
  serviceName = "jellyfin";
  jellyfinUrlBase = "/jellyfin";
  jellyfinRootUrl = "http://${config.custom.shared.localHostIPv4}:${toString config.custom.servicePort.jellyfin}";
  jellyfinBaseUrl = "${jellyfinRootUrl}${jellyfinUrlBase}";
  mediaDirectory = config.custom.shared.pathToMediaDirectory;
  moviesRootFolder = "${mediaDirectory}/movies";
  tvRootFolder = "${mediaDirectory}/tv";
  usernameCredential = "jellyfin-username";
  passwordCredential = "jellyfin-password";
in
{
  options.custom = {
    servicePort.jellyfin = lib.mkOption {
      type = lib.types.int;
      default = 8096;
      description = "Jellyfin port";
    };
  };

  config = lib.mkIf config.services.jellyfin.enable {
    sops.secrets."${serviceName}/username" = {
      restartUnits = [
        config.systemd.services.jellyfin-setup-wizard.name
        config.systemd.services.jellyfin-libraries.name
      ];
    };

    sops.secrets."${serviceName}/password" = {
      restartUnits = [
        config.systemd.services.jellyfin-setup-wizard.name
        config.systemd.services.jellyfin-libraries.name
      ];
    };

    environment.systemPackages = with pkgs; [
      jellyfin
      jellyfin-web
      jellyfin-ffmpeg
    ];

    services.jellyfin = {
      openFirewall = true;
      group = config.custom.shared.mediaGroup;
    };

    systemd.services.jellyfin-setup-wizard = {
      description = "Complete Jellyfin setup wizard";
      after = [ config.systemd.services.jellyfin.name ];
      requires = [ config.systemd.services.jellyfin.name ];
      wantedBy = [ config.systemd.services.jellyfin.name ];

      path = with pkgs; [
        coreutils
        curl
        gnused
        jq
      ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        LoadCredential = [
          "${usernameCredential}:${config.sops.secrets."${serviceName}/username".path}"
          "${passwordCredential}:${config.sops.secrets."${serviceName}/password".path}"
        ];
      };

      script = ''
        set -euo pipefail

        ROOT_URL=${lib.escapeShellArg jellyfinRootUrl}
        BASE_URL=${lib.escapeShellArg jellyfinBaseUrl}
        USERNAME_CREDENTIAL="$CREDENTIALS_DIRECTORY/${usernameCredential}"
        PASSWORD_CREDENTIAL="$CREDENTIALS_DIRECTORY/${passwordCredential}"
        SERVER_NAME=${lib.escapeShellArg config.networking.hostName}

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

        echo "Waiting for Jellyfin API..."
        for attempt in $(seq 1 60); do
          for candidate_base_url in "$BASE_URL" "$ROOT_URL"; do
            if SYSTEM_INFO=$(curl -fsS "$candidate_base_url/System/Info/Public" 2>/dev/null) \
              && printf '%s' "$SYSTEM_INFO" | jq -e 'type == "object" and has("StartupWizardCompleted")' >/dev/null
            then
              SETUP_BASE_URL="$candidate_base_url"
              break 2
            fi
          done

          if [ "$attempt" -eq 60 ]; then
            echo "Timed out waiting for Jellyfin API" >&2
            exit 1
          fi

          sleep 2
        done

        STARTUP_WIZARD_COMPLETED=$(printf '%s' "$SYSTEM_INFO" | jq -r '.StartupWizardCompleted // false')

        if [ "$STARTUP_WIZARD_COMPLETED" != "false" ]; then
          echo "Jellyfin setup wizard already completed"
          exit 0
        fi

        echo "Running Jellyfin setup wizard"

        request() {
          local description="$1"
          local url="$2"
          shift 2
          local response http_code body

          response=$(curl -sS -w "\n%{http_code}" "$@" "$url")
          http_code=$(printf '%s\n' "$response" | tail -n1)
          body=$(printf '%s\n' "$response" | sed '$d')

          if [ "$http_code" -lt 200 ] || [ "$http_code" -ge 300 ]; then
            echo "Failed to $description (HTTP $http_code)" >&2
            if [ -n "$body" ]; then
              echo "$body" >&2
            fi
            exit 1
          fi
        }

        CONFIGURATION_PAYLOAD=$(jq -n \
          --arg serverName "$SERVER_NAME" \
          '{
            ServerName: $serverName,
            UICulture: "en-US",
            MetadataCountryCode: "US",
            PreferredMetadataLanguage: "en"
          }')

        request "set initial configuration" "$SETUP_BASE_URL/Startup/Configuration" \
          -X POST \
          -H "Content-Type: application/json" \
          --data-binary "$CONFIGURATION_PAYLOAD"

        curl -fsS "$SETUP_BASE_URL/Startup/User" >/dev/null

        new_temp_file USER_PAYLOAD
        jq -n \
          --rawfile username "$USERNAME_CREDENTIAL" \
          --rawfile password "$PASSWORD_CREDENTIAL" \
          '{
            Name: ($username | rtrimstr("\n")),
            Password: ($password | rtrimstr("\n"))
          }' > "$USER_PAYLOAD"

        request "create initial Jellyfin user" "$SETUP_BASE_URL/Startup/User" \
          -X POST \
          -H "Content-Type: application/json" \
          --data-binary "@$USER_PAYLOAD"

        request "configure remote access" "$SETUP_BASE_URL/Startup/RemoteAccess" \
          -X POST \
          -H "Content-Type: application/json" \
          --data-binary '{"EnableRemoteAccess":true}'

        request "complete setup wizard" "$SETUP_BASE_URL/Startup/Complete" \
          -X POST

        echo "Jellyfin setup wizard completed"
      '';
    };

    systemd.services.jellyfin-libraries = {
      description = "Configure Jellyfin libraries";
      after = [
        config.systemd.services.jellyfin.name
        config.systemd.services.jellyfin-setup-wizard.name
        "systemd-tmpfiles-setup.service"
      ];
      requires = [
        config.systemd.services.jellyfin.name
        config.systemd.services.jellyfin-setup-wizard.name
        "systemd-tmpfiles-setup.service"
      ];
      wantedBy = [ config.systemd.services.jellyfin.name ];

      path = with pkgs; [
        coreutils
        curl
        gnused
        jq
      ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        LoadCredential = [
          "${usernameCredential}:${config.sops.secrets."${serviceName}/username".path}"
          "${passwordCredential}:${config.sops.secrets."${serviceName}/password".path}"
        ];
      };

      script = ''
        set -euo pipefail

        ROOT_URL=${lib.escapeShellArg jellyfinRootUrl}
        BASE_URL=${lib.escapeShellArg jellyfinBaseUrl}
        USERNAME_CREDENTIAL="$CREDENTIALS_DIRECTORY/${usernameCredential}"
        PASSWORD_CREDENTIAL="$CREDENTIALS_DIRECTORY/${passwordCredential}"
        MOVIES_ROOT_FOLDER=${lib.escapeShellArg moviesRootFolder}
        TV_ROOT_FOLDER=${lib.escapeShellArg tvRootFolder}
        MEDIA_GROUP=${lib.escapeShellArg config.custom.shared.mediaGroup}
        JELLYFIN_USER=${lib.escapeShellArg config.services.jellyfin.user}
        AUTHORIZATION_HEADER='Authorization: MediaBrowser Client="nixos", Device="nixos", DeviceId="nixos-jellyfin-libraries", Version="1.0.0"'

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

        new_temp_file AUTH_PAYLOAD
        jq -n \
          --rawfile username "$USERNAME_CREDENTIAL" \
          --rawfile password "$PASSWORD_CREDENTIAL" \
          '{
            Username: ($username | rtrimstr("\n")),
            Pw: ($password | rtrimstr("\n"))
          }' > "$AUTH_PAYLOAD"

        AUTH_HTTP_CODE=""
        AUTH_BODY=""

        echo "Waiting for Jellyfin authentication..."
        for attempt in $(seq 1 60); do
          for candidate_base_url in "$BASE_URL" "$ROOT_URL"; do
            AUTH_RESPONSE=$(curl -sS -w "\n%{http_code}" \
              -X POST \
              -H "$AUTHORIZATION_HEADER" \
              -H "Content-Type: application/json" \
              --data-binary "@$AUTH_PAYLOAD" \
              "$candidate_base_url/Users/AuthenticateByName" || true)

            AUTH_HTTP_CODE=$(printf '%s\n' "$AUTH_RESPONSE" | tail -n1)
            AUTH_BODY=$(printf '%s\n' "$AUTH_RESPONSE" | sed '$d')

            if [ "$AUTH_HTTP_CODE" = "200" ]; then
              ACCESS_TOKEN=$(printf '%s' "$AUTH_BODY" | jq -r '.AccessToken // empty')
              if [ -n "$ACCESS_TOKEN" ]; then
                ACTIVE_BASE_URL="$candidate_base_url"
                break 2
              fi
            fi
          done

          if [ "$attempt" -eq 60 ]; then
            echo "Timed out waiting for Jellyfin authentication" >&2
            echo "Last auth HTTP code: ''${AUTH_HTTP_CODE:-none}" >&2
            if [ "''${AUTH_HTTP_CODE:-}" != "200" ] && [ -n "''${AUTH_BODY:-}" ]; then
              echo "Last auth response body:" >&2
              echo "$AUTH_BODY" >&2
            fi
            exit 1
          fi

          sleep 2
        done

        new_temp_file CURL_CONFIG
        printf 'header = "X-Emby-Token: %s"\n' "$ACCESS_TOKEN" > "$CURL_CONFIG"

        curl_jellyfin() {
          local url="$1"
          shift

          curl -fsS -K "$CURL_CONFIG" "$@" "$url"
        }

        request_jellyfin() {
          local description="$1"
          local url="$2"
          shift 2
          local response http_code body

          response=$(curl -sS -K "$CURL_CONFIG" -w "\n%{http_code}" "$@" "$url")
          http_code=$(printf '%s\n' "$response" | tail -n1)
          body=$(printf '%s\n' "$response" | sed '$d')

          if [ "$http_code" -lt 200 ] || [ "$http_code" -ge 300 ]; then
            echo "Failed to $description (HTTP $http_code)" >&2
            if [ -n "$body" ]; then
              echo "$body" >&2
            fi
            exit 1
          fi
        }

        url_encode() {
          jq -rn --arg value "$1" '$value | @uri'
        }

        ensure_directory() {
          local path="$1"

          if [ ! -d "$path" ]; then
            install -d -m 0770 -o "$JELLYFIN_USER" -g "$MEDIA_GROUP" "$path"
          fi
        }

        ensure_library() {
          local name="$1"
          local collection_type="$2"
          local path="$3"
          local encoded_name existing_library existing_collection_type existing_paths create_payload add_path_payload

          ensure_directory "$path"

          existing_library=$(printf '%s' "$LIBRARIES_JSON" | jq -c --arg name "$name" '.[] | select(.Name == $name)' | head -n1)

          if [ -z "$existing_library" ]; then
            echo "Creating Jellyfin library $name -> $path"
            encoded_name=$(url_encode "$name")
            create_payload=$(jq -n --arg path "$path" '{LibraryOptions: {PathInfos: [{Path: $path}]}}')

            request_jellyfin "create Jellyfin library $name" \
              "$ACTIVE_BASE_URL/Library/VirtualFolders?name=$encoded_name&collectionType=$collection_type&refreshLibrary=true" \
              -X POST \
              -H "Content-Type: application/json" \
              --data-binary "$create_payload"
            return
          fi

          existing_collection_type=$(printf '%s' "$existing_library" | jq -r '.CollectionType // empty')
          if [ "$existing_collection_type" != "$collection_type" ]; then
            echo "Jellyfin library $name exists with collection type $existing_collection_type, expected $collection_type" >&2
            exit 1
          fi

          existing_paths=$(printf '%s' "$existing_library" | jq -c '.Locations // []')
          if printf '%s' "$existing_paths" | jq -e --arg path "$path" 'index($path)' >/dev/null; then
            echo "Jellyfin library $name already includes $path"
            return
          fi

          echo "Adding $path to Jellyfin library $name"
          add_path_payload=$(jq -n --arg name "$name" --arg path "$path" '{Name: $name, Path: $path}')
          request_jellyfin "add $path to Jellyfin library $name" \
            "$ACTIVE_BASE_URL/Library/VirtualFolders/Paths?refreshLibrary=true" \
            -X POST \
            -H "Content-Type: application/json" \
            --data-binary "$add_path_payload"
        }

        echo "Fetching Jellyfin libraries"
        LIBRARIES_JSON=$(curl_jellyfin "$ACTIVE_BASE_URL/Library/VirtualFolders")

        ensure_library "Movies" "movies" "$MOVIES_ROOT_FOLDER"
        LIBRARIES_JSON=$(curl_jellyfin "$ACTIVE_BASE_URL/Library/VirtualFolders")
        ensure_library "Shows" "tvshows" "$TV_ROOT_FOLDER"

        echo "Jellyfin libraries configured"
      '';
    };
  };
}
