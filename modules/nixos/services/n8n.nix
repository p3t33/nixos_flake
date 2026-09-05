{ config, lib, ... }:
let
  appsDomain = config.custom.shared.appsDomain;
  n8nHost = "n8n.${appsDomain}";
  externalScheme = if config.custom.security.acme.enable then "https" else "http";
  externalUrl = "${externalScheme}://${n8nHost}/";
in
{
  options.custom.services.n8n.port = lib.mkOption {
    type = lib.types.port;
    default = 5678;
    description = "Internal n8n HTTP port.";
  };

  config = lib.mkIf config.services.n8n.enable {
      sops.secrets.n8n-env = {};

      services.n8n = {
        # These remain in the Nix store (visible)
        environment = {
          N8N_PORT = toString config.custom.services.n8n.port;
          N8N_LISTEN_ADDRESS = config.custom.shared.localHostIPv4;
          N8N_HOST = n8nHost;
          N8N_PROTOCOL = externalScheme;
          N8N_EDITOR_BASE_URL = externalUrl;
          N8N_WEBHOOK_URL = externalUrl;
          N8N_PROXY_HOPS = "1";
          GENERIC_TIMEZONE = "Asia/Jerusalem";
          DB_TYPE = "postgresdb";
          DB_POSTGRESDB_HOST = "/run/postgresql";
          DB_POSTGRESDB_PORT = "${toString config.services.postgresql.settings.port}";
          DB_POSTGRESDB_DATABASE = "n8n";
          DB_POSTGRESDB_USER = "n8n";
          N8N_DIAGNOSTICS_ENABLED = "false";
          N8N_VERSION_NOTIFICATIONS_ENABLED = "false";
          N8N_ENFORCE_SETTINGS_FILE_PERMISSIONS = "true";
          N8N_SECURE_COOKIE = if config.custom.security.acme.enable then "true" else "false";
        };
      };

    # This is how you add the secret environment file
    systemd.services.n8n.serviceConfig = {
      # This path points to your sops-nix secret or a manual path
      EnvironmentFile = config.sops.secrets.n8n-env.path;
      UMask = "0077";
    };
  };
}
