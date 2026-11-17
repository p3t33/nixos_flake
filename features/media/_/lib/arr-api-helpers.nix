{ lib }:
{
  # Temp file management with automatic cleanup.
  # Sets up TMP_FILES array, cleanup trap, new_temp_file, and new_curl_config.
  common = ''
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

    new_secret_file() {
      local result_var="$1"
      local value="$2"
      local file

      new_temp_file file
      printf '%s' "$value" > "$file"
      printf -v "$result_var" '%s' "$file"
    }
  '';

  # Generate a named curl wrapper function.
  # name: used in the function name (e.g. "sonarr" -> curl_sonarr)
  # configVar: shell variable holding the curl config file path
  mkCurlWrapper = name: configVar: ''
    curl_${name}() {
      local url="$1"
      shift

      curl -fsS -K "''$${configVar}" "$@" "$url"
    }
  '';

  # Generic upsert function for Arr download clients.
  # Expects SCHEMAS, DOWNLOAD_CLIENTS, BASE_URL, and a curl wrapper to be set up
  # by the caller. The curl function name is passed as the 4th argument.
  upsertClient = ''
    upsert_client() {
      local client_name="$1"
      local implementation_name="$2"
      local build_payload_fn="$3"
      local curl_fn="$4"

      local existing_client
      existing_client=$(echo "$DOWNLOAD_CLIENTS" | jq -c --arg name "$client_name" '.[] | select(.name == $name)' | head -n1)

      if [ -n "$existing_client" ] && [ "$(echo "$existing_client" | jq -r '.implementationName')" != "$implementation_name" ]; then
        echo "Download client named $client_name exists but is not $implementation_name" >&2
        return 1
      fi

      if [ -n "$existing_client" ]; then
        local client_id
        client_id=$(echo "$existing_client" | jq -r '.id')
        local payload
        payload=$(echo "$existing_client" | $build_payload_fn)

        printf '%s' "$payload" | $curl_fn "$BASE_URL/downloadclient/$client_id" \
          -X PUT \
          -H "Content-Type: application/json" \
          --data-binary @- \
          >/dev/null

        echo "Updated download client: $client_name"
      else
        local schema
        schema=$(echo "$SCHEMAS" | jq -c --arg impl "$implementation_name" '.[] | select(.implementationName == $impl)' | head -n1)

        if [ -z "$schema" ]; then
          echo "No schema found for $implementation_name" >&2
          return 1
        fi

        local payload
        payload=$(echo "$schema" | $build_payload_fn)

        printf '%s' "$payload" | $curl_fn "$BASE_URL/downloadclient" \
          -X POST \
          -H "Content-Type: application/json" \
          --data-binary @- \
          >/dev/null

        echo "Created download client: $client_name"
      fi
    }
  '';
}
