{ config, lib, ... }:
{
  services = {
    zfs.autoScrub.enable = true;
    nginx.enable = true;
    syncthing.enable = true;
    homepage-dashboard.enable = true;
    calibre-web.enable = true;
    audiobookshelf.enable = true;
    jellyfin.enable = true;
    kavita.enable = true;
    prowlarr.enable = true;
    # jackett.enable = true; # No real need for now
    sonarr.enable = true;
    radarr.enable = true;
    recyclarr.enable = true;
  # bazarr.enable = true; # needs to be added at a later point
    deluge.enable = true;
    sabnzbd.enable = true;
    gatus.enable = true;
    samba.enable = true;
    paperless.enable = true;
    postgresql.enable = true;
    postgresqlBackup.enable = true;
    immich.enable = true;
    n8n.enable = false; # Revisit on NixOS 26.11 upgrade: pnpm dependency hash mismatch.
    minecraft-server.enable = true;
  };

  systemd.tmpfiles.rules = lib.mkIf config.services.syncthing.enable [
      "d ${config.custom.services.syncthing.syncDir} 0770 ${config.services.syncthing.user} ${config.custom.shared.dataGroup} -"
  ];

  custom = {
    security.acme.enable = true;

    profiles.systemServices = {
      core.enable = true;
      server.enable = true;
      monitoring.enable = true;
    };

    services = {
      avahi.publisher.enable  = true;
      printing.server.enable  = true;
      restic.enable = true;
      qbittorrent.enable = true;

      sftpBackup = {
        enable = true;
        authorizedKeys = [
          ''from="192.168.1.1" ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIXwzY+c2nMYs5xNTi+KLfRbMLorWkVrHc/R0M8SN51f router-network-backup''
        ];
      };

      syncthing = {
        syncDir = "${config.custom.shared.pathToDataDirectory}/syncthing";
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
                "${config.services.syncthing.settings.devices.home-desktop.name}"
                "${config.services.syncthing.settings.devices.work-pc.name}"
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
                "${config.services.syncthing.settings.devices.home-desktop.name}"
                "${config.services.syncthing.settings.devices.work-pc.name}"
              ];
            };
          };
        };
      };
    };
  };
}
