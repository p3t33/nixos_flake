{
  config,
  lib,
  pkgs,
  ...
}:

let
  serviceName = "audiobookshelf";
  cfg = config.services.${serviceName};
  mediaDirectory = config.custom.shared.pathToMediaDirectory;
  audiobookshelfDirectory = "${mediaDirectory}/audiobookshelf";
  audiobooksDirectory = "${audiobookshelfDirectory}/audiobooks";
  podcastsDirectory = "${audiobookshelfDirectory}/podcasts";
  apiHost =
    if cfg.host == "0.0.0.0" then
      "127.0.0.1"
    else if cfg.host == "::" then
      "[::1]"
    else
      cfg.host;
  apiBaseUrl = "http://${apiHost}:${toString cfg.port}";
  usernameCredential = "audiobookshelf-username";
  passwordCredential = "audiobookshelf-password";
in
{
  config = lib.mkIf cfg.enable {
    sops.secrets."${serviceName}/username".restartUnits = [
      config.systemd.services.audiobookshelf-setup.name
    ];
    sops.secrets."${serviceName}/password".restartUnits = [
      config.systemd.services.audiobookshelf-setup.name
    ];

    systemd.tmpfiles.rules = [
      "d ${audiobookshelfDirectory} 2770 ${cfg.user} ${config.custom.shared.mediaGroup} -"
      "d ${audiobooksDirectory} 2770 ${cfg.user} ${config.custom.shared.mediaGroup} -"
      "d ${podcastsDirectory} 2770 ${cfg.user} ${config.custom.shared.mediaGroup} -"
    ];

    services.audiobookshelf = {
      host = config.custom.shared.anyIPv4;
      openFirewall = true;
      group = config.custom.shared.mediaGroup;
    };

    systemd.services.audiobookshelf-setup = {
      description = "Initialize and configure Audiobookshelf";
      after = [
        config.systemd.services.audiobookshelf.name
        "local-fs.target"
        "systemd-tmpfiles-setup.service"
      ];
      requires = [ "systemd-tmpfiles-setup.service" ];
      wantedBy = [ "multi-user.target" ];

      unitConfig = {
        RequiresMountsFor = [ audiobookshelfDirectory ];
      };

      path = with pkgs; [
        coreutils
        curl
        jq
      ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        RuntimeDirectory = "audiobookshelf-setup";
        RuntimeDirectoryMode = "0700";
        UMask = "0077";
        LoadCredential = [
          "${usernameCredential}:${config.sops.secrets."${serviceName}/username".path}"
          "${passwordCredential}:${config.sops.secrets."${serviceName}/password".path}"
        ];
      };

      script = ''
        set -euo pipefail
        umask 0077

        BASE_URL=${lib.escapeShellArg apiBaseUrl}
        USERNAME_CREDENTIAL="$CREDENTIALS_DIRECTORY/${usernameCredential}"
        PASSWORD_CREDENTIAL="$CREDENTIALS_DIRECTORY/${passwordCredential}"
        AUDIOBOOKS_DIRECTORY=${lib.escapeShellArg audiobooksDirectory}
        PODCASTS_DIRECTORY=${lib.escapeShellArg podcastsDirectory}
        SETUP_ATTEMPTS="''${AUDIOBOOKSHELF_SETUP_ATTEMPTS:-60}"
        SETUP_SLEEP_SECONDS="''${AUDIOBOOKSHELF_SETUP_SLEEP_SECONDS:-1}"

        TMP_FILES=()
        LOGGED_IN=0

        cleanup() {
          local tmp_file

          if [ "$LOGGED_IN" -eq 1 ] && [ -f "$COOKIE_JAR" ]; then
            curl --disable --noproxy '*' --connect-timeout 2 --max-time 5 --max-redirs 0 \
              --proto '=http' -sS -o /dev/null -b "$COOKIE_JAR" -X POST "$BASE_URL/logout" || true
          fi

          for tmp_file in "''${TMP_FILES[@]}"; do
            rm -f "$tmp_file"
          done
        }
        trap cleanup EXIT

        fail() {
          echo "Audiobookshelf setup failed: $1" >&2
          exit 1
        }

        new_temp_file() {
          local result_var="$1"
          local tmp_file

          tmp_file=$(mktemp "$RUNTIME_DIRECTORY/audiobookshelf.XXXXXX")
          TMP_FILES+=("$tmp_file")
          printf -v "$result_var" '%s' "$tmp_file"
        }

        request() {
          local description="$1"
          local response_file="$2"
          local endpoint="$3"
          local http_code
          shift 3

          http_code=$(curl --disable --noproxy '*' --connect-timeout 2 --max-time 5 --max-redirs 0 \
            --proto '=http' -sS -o "$response_file" -w '%{http_code}' "$@" "$BASE_URL$endpoint" || true)

          case "$http_code" in
            2??) ;;
            *) fail "$description (HTTP ''${http_code:-000})" ;;
          esac
        }

        new_temp_file CREDENTIALS_PAYLOAD
        jq -n \
          --rawfile username "$USERNAME_CREDENTIAL" \
          --rawfile password "$PASSWORD_CREDENTIAL" \
          '{
            username: (if ($username | endswith("\n")) then $username[0:-1] else $username end),
            password: (if ($password | endswith("\n")) then $password[0:-1] else $password end)
          }' > "$CREDENTIALS_PAYLOAD"

        if ! jq -e '(.username | type == "string" and length > 0) and (.password | type == "string" and length > 0)' \
          "$CREDENTIALS_PAYLOAD" >/dev/null
        then
          fail "username and password credentials must be nonempty"
        fi

        new_temp_file STATUS_RESPONSE
        STATUS_HTTP_CODE=""
        STATUS_VALID=0
        for attempt in $(seq 1 "$SETUP_ATTEMPTS"); do
          STATUS_HTTP_CODE=$(curl --disable --noproxy '*' --connect-timeout 2 --max-time 5 --max-redirs 0 \
            --proto '=http' -sS -o "$STATUS_RESPONSE" -w '%{http_code}' "$BASE_URL/status" || true)

          if [ "$STATUS_HTTP_CODE" = "200" ]; then
            if jq -e 'type == "object" and .app == "audiobookshelf" and (.isInit | type == "boolean")' \
              "$STATUS_RESPONSE" >/dev/null
            then
              STATUS_VALID=1
              break
            fi

            fail "received malformed status response"
          fi

          if [ "$attempt" -lt "$SETUP_ATTEMPTS" ]; then
            sleep "$SETUP_SLEEP_SECONDS"
          fi
        done

        if [ "$STATUS_VALID" -ne 1 ]; then
          fail "service did not become ready (last HTTP ''${STATUS_HTTP_CODE:-000})"
        fi

        IS_INIT=$(jq -r '.isInit' "$STATUS_RESPONSE")
        if [ "$IS_INIT" = "false" ]; then
          new_temp_file INIT_PAYLOAD
          jq '{ newRoot: . }' "$CREDENTIALS_PAYLOAD" > "$INIT_PAYLOAD"
          new_temp_file INIT_RESPONSE
          request "initialize root user" "$INIT_RESPONSE" "/init" \
            -X POST -H "Content-Type: application/json" --data-binary "@$INIT_PAYLOAD"
        fi

        new_temp_file COOKIE_JAR
        new_temp_file LOGIN_RESPONSE
        request "log in as root user" "$LOGIN_RESPONSE" "/login" \
          -c "$COOKIE_JAR" -X POST -H "Content-Type: application/json" --data-binary "@$CREDENTIALS_PAYLOAD"
        LOGGED_IN=1

        if ! jq -e '.user | type == "object" and .type == "root" and (.accessToken | type == "string" and length > 0)' \
          "$LOGIN_RESPONSE" >/dev/null
        then
          fail "login did not return a root access token"
        fi

        new_temp_file CURL_CONFIG
        jq -r '.user.accessToken | "header = \"Authorization: Bearer \(.)\""' "$LOGIN_RESPONSE" > "$CURL_CONFIG"

        new_temp_file LIBRARIES_RESPONSE
        request "fetch libraries" "$LIBRARIES_RESPONSE" "/api/libraries" -K "$CURL_CONFIG"
        if ! jq -e '.libraries | type == "array"' "$LIBRARIES_RESPONSE" >/dev/null; then
          fail "received malformed libraries response"
        fi

        ensure_library() {
          local name="$1"
          local media_type="$2"
          local icon="$3"
          local provider="$4"
          local folder="$5"
          local matching_libraries folder_owners payload response

          matching_libraries=$(jq -c --arg name "$name" '[.libraries[] | select(.name == $name)]' "$LIBRARIES_RESPONSE")
          if [ "$(printf '%s' "$matching_libraries" | jq 'length')" -gt 1 ]; then
            fail "multiple libraries are named $name"
          fi

          folder_owners=$(jq -c --arg name "$name" --arg folder "$folder" \
            '[.libraries[] | select(.name != $name and ((.folders // []) | any(.[]; .fullPath == $folder))) | .name]' "$LIBRARIES_RESPONSE")
          if [ "$(printf '%s' "$folder_owners" | jq 'length')" -gt 0 ]; then
            fail "$folder is already owned by another library"
          fi

          if [ "$(printf '%s' "$matching_libraries" | jq 'length')" -eq 1 ]; then
            if ! printf '%s' "$matching_libraries" | jq -e --arg media_type "$media_type" \
              '.[0].mediaType == $media_type' >/dev/null
            then
              fail "library $name has a conflicting media type"
            fi

            if ! printf '%s' "$matching_libraries" | jq -e --arg folder "$folder" \
              '.[0].folders | type == "array" and any(.[]; .fullPath == $folder)' >/dev/null
            then
              fail "library $name does not own $folder"
            fi

            echo "Audiobookshelf library $name already configured"
            return
          fi

          new_temp_file payload
          jq -n \
            --arg name "$name" \
            --arg media_type "$media_type" \
            --arg icon "$icon" \
            --arg provider "$provider" \
            --arg folder "$folder" \
            '{ name: $name, mediaType: $media_type, icon: $icon, provider: $provider, folders: [{ fullPath: $folder }] }' \
            > "$payload"
          new_temp_file response
          request "create library $name" "$response" "/api/libraries" \
            -K "$CURL_CONFIG" -X POST -H "Content-Type: application/json" --data-binary "@$payload"
          echo "Created Audiobookshelf library $name"
        }

        ensure_library "Audiobooks" "book" "audiobookshelf" "audible" "$AUDIOBOOKS_DIRECTORY"
        ensure_library "Podcasts" "podcast" "audiobookshelf" "itunes" "$PODCASTS_DIRECTORY"

        echo "Audiobookshelf setup completed"
      '';
    };
  };
}
