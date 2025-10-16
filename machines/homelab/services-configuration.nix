{ config, ... }:
{
  imports = [
    ../../modules/nixos/services # imported via default.nix
  ];

  services = {
    nginx.enable = true;
    syncthing.enable = true;
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
    inadyn.enable = true;
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

    vpn.wireguardServer.enable = true;

    services = {
      restic.enable = true;

      syncthing = {
        syncDir = "/mnt/data/Sync";
        user = "syncthing";
        simpleFileVersioningForBackUpMachinesOnly = {
          type = "simple";
          params = {
            keep = "5";
            cleanoutDays = "10";
          };
          cleanupIntervalS = 3600;
        };

        settings = {
          devices = {
            home-desktop.enable = true;
            work-pc.enable = true;
          };

          folders = {
            taskwarrior = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.work-pc.name}"
                "${config.services.syncthing.settings.devices.home-desktop.name}"
              ];
            };

            database = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.home-desktop.name}"
              ];
            };

            documents = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.home-desktop.name}"
              ];
            };

            study = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.home-desktop.name}"
              ];
            };

            dev_resources = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.work-pc.name}"
                "${config.services.syncthing.settings.devices.home-desktop.name}"
              ];
            };
          };
        };
      };
    };
  };
}
