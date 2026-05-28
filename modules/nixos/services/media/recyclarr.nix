{ config, lib, pkgs-unstable, ... }:

let
  sonarrBaseUrl = "http://${config.custom.shared.localHostIPv4}:${toString config.services.sonarr.settings.server.port}${config.services.sonarr.settings.server.urlbase}";
  radarrBaseUrl = "http://${config.custom.shared.localHostIPv4}:${toString config.services.radarr.settings.server.port}${config.services.radarr.settings.server.urlbase}";
  configPath = "/var/lib/recyclarr/recyclarr.yml";
in
{
  config = lib.mkIf config.services.recyclarr.enable {
    sops.secrets."sonarr/api_key" = { };
    sops.secrets."radarr/api_key" = { };

    systemd.tmpfiles.rules = [
      "d /var/lib/recyclarr 0750 recyclarr recyclarr -"
    ];

    sops.templates."recyclarr.yml" = {
      path = configPath;
      owner = "recyclarr";
      mode = "0640";
      # This is a minimal subset — only the settings we explicitly own.
      # Recyclarr creates the named profiles in Sonarr/Radarr on each run.
      # Assign profiles to series/movies manually in the UI.
      content = ''
        sonarr:
          sonarr:
            base_url: ${sonarrBaseUrl}
            api_key: ${config.sops.placeholder."sonarr/api_key"}
            delete_old_custom_formats: false
            quality_definition:
              type: series
            quality_profiles:
              - trash_id: dfa5eaae7894077ad6449169b6eb03e0
                name: "[TRaSH] WEB-2160p"
                reset_unmatched_scores:
                  enabled: false
              - trash_id: 9d142234e45d6143785ac55f5a9e8dc9
                name: "[TRaSH] WEB-1080p"
                reset_unmatched_scores:
                  enabled: false

        radarr:
          radarr:
            base_url: ${radarrBaseUrl}
            api_key: ${config.sops.placeholder."radarr/api_key"}
            delete_old_custom_formats: false
            quality_definition:
              type: sqp-streaming
            quality_profiles:
              - trash_id: 5128baeb2b081b72126bc8482b2a86a0
                name: "[TRaSH] SQP-1 (2160p)"
                reset_unmatched_scores:
                  enabled: false
              - trash_id: 0896c29d74de619df168d23b98104b22
                name: "[TRaSH] SQP-1 (1080p)"
                reset_unmatched_scores:
                  enabled: false
      '';
    };

    services.recyclarr = {
      package = pkgs-unstable.recyclarr;
      schedule = "daily";
    };

    systemd.services.recyclarr = {
      environment.RECYCLARR_CONFIG_DIR = "/var/lib/recyclarr";
      serviceConfig.ExecStart = lib.mkForce "${lib.getExe pkgs-unstable.recyclarr} sync --config ${configPath}";
      after = [
        "sonarr.service"
        "radarr.service"
      ];
      requires = [
        "sonarr.service"
        "radarr.service"
      ];
    };
  };
}
