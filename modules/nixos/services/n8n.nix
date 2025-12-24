{ config, lib, ... }:
{

  config = lib.mkIf config.services.n8n.enable {
      sops.secrets.n8n-env = {};

      services.n8n = {
        # These remain in the Nix store (visible)
        environment = {
          N8N_PORT = "5678";
          GENERIC_TIMEZONE = "Asia/Jerusalem";
          DB_TYPE = "postgresdb";
          DB_POSTGRESDB_HOST = "/run/postgresql";
          DB_POSTGRESDB_PORT = "${toString config.services.postgresql.settings.port}";
          DB_POSTGRESDB_DATABASE = "n8n";
          DB_POSTGRESDB_USER = "n8n";
          N8N_DIAGNOSTICS_ENABLED = "false";
          N8N_VERSION_NOTIFICATIONS_ENABLED = "false";
          N8N_ENFORCE_SETTINGS_FILE_PERMISSIONS = "true";
          N8N_SECURE_COOKIE = "false";
        };
      };

    # This is how you add the secret environment file
    networking.firewall.allowedTCPPorts = [
      (lib.toInt config.services.n8n.environment.N8N_PORT)
    ];
    systemd.services.n8n.serviceConfig = {
      # This path points to your sops-nix secret or a manual path
      EnvironmentFile = config.sops.secrets.n8n-env.path;
      UMask = "0077";
    };
  };
}
