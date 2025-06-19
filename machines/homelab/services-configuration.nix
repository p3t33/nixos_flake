{ config, ... }:
{
  imports = [
    ../../modules/nixos/services # imported via default.nix
  ];

  services.nginx.enable = true;
  services.syncthing.enable = true;
  services.adguardhome.enable = true;
  services.homepage-dashboard.enable = true;
  services.calibre-web.enable = true;
  services.jellyfin.enable = true;
  services.prowlarr.enable = true;
  services.jackett.enable = true;
  services.sonarr.enable = true;
  services.radarr.enable = true;
  services.readarr.enable = true;
  services.bazarr.enable = true;
  services.deluge.enable = true;
  services.sabnzbd.enable = true;
  services.inadyn.enable = true;
  services.gatus.enable = true;
  services.prometheus.enable = true;
  services.promtail.enable = true;
  services.loki.enable = true;
  services.grafana.enable = true;
  services.samba.enable = true;
  services.paperless.enable = true;
  services.postgresql.enable = true;
  services.postgresqlBackup.enable = true;

  custom = {
    profiles.systemServices = {
      core = true;
      server = true;
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

        remoteDevices = {
          home-desktop.enable = true;
          work-pc.enable = true;
        };

        foldersToShare = {

          taskwarrior = {
            enable = true;
            devicesToShareWith = [
              "${config.custom.services.syncthing.remoteDevices.work-pc.name}"
              "${config.custom.services.syncthing.remoteDevices.home-desktop.name}"
            ];
          };

          database = {
            enable = true;
            devicesToShareWith = [
              "${config.custom.services.syncthing.remoteDevices.home-desktop.name}"
            ];
          };

          documents = {
            enable = true;
            devicesToShareWith = [
              "${config.custom.services.syncthing.remoteDevices.home-desktop.name}"
            ];
          };

          study = {
            enable = true;
            devicesToShareWith = [
              "${config.custom.services.syncthing.remoteDevices.home-desktop.name}"
            ];
          };

          devResources = {
            enable = true;
            devicesToShareWith = [
              "${config.custom.services.syncthing.remoteDevices.work-pc.name}"
              "${config.custom.services.syncthing.remoteDevices.home-desktop.name}"
            ];
          };
        };

      };
    };
  };
}
