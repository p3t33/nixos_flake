{ config, ... }:
{
  imports = [
    ../../modules/nixos/services # imported via default.nix
  ];

  services = {
    nginx.enable = true;
    adguardhome.enable = true;
    homepage-dashboard.enable = true;
    calibre-web.enable = true;
    jellyfin.enable = true;
    prowlarr.enable = true;
    jackett.enable = true;
    sonarr.enable = true;
    radarr.enable = true;
    bazarr.enable = true;
    deluge.enable = true;
    sabnzbd.enable = true;
    gatus.enable = true;
    prometheus.enable = true;
    promtail.enable = true;
    loki.enable = true;
    grafana.enable = true;
    samba.enable = true;
    paperless.enable = true;
    postgresql.enable = true;
    postgresqlBackup.enable = true;
  };

  custom = {
    profiles.systemServices = {
      core.enable = true;
      server.enable = true;
    };
  };
}
