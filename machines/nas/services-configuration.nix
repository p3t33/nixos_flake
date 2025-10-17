{ config, lib, ... }:
{

  imports = [
    ../../modules/nixos/services # imported via default.nix
  ];

  services = {
    zfs.autoScrub.enable = true;
    nginx.enable = true;
    syncthing.enable = true;
    adguardhome.enable = true;
    homepage-dashboard.enable = true;
    calibre-web.enable = true;
    jellyfin.enable = true;
    prowlarr.enable = true;
    # jackett.enable = true; # No real need for now
    sonarr.enable = true;
    radarr.enable = true;
  # bazarr.enable = true; # needs to be added at a later point
    deluge.enable = true;
    sabnzbd.enable = true;
  # inadyn.enable = true; # will be added as part of the vpn.
    gatus.enable = true;
    # prometheus.enable = true; # Not sure I want or need this.
    # promtail.enable = true;  # Not sure I want or need this.
    # loki.enable = true; # Not sure I want or need this.
    # grafana.enable = true;  # Not sure I want or need this.
    samba.enable = true;
  # paperless.enable = true; # This needs to be worked on.
    postgresql.enable = true;
    postgresqlBackup.enable = true;
  };

  systemd.tmpfiles.rules = lib.mkIf config.services.syncthing.enable [
      "d ${config.custom.services.syncthing.syncDir} 0770 ${config.services.syncthing.user} ${config.customGlobal.dataGroup} -"
  ];

  custom = {
    profiles.systemServices = {
      core.enable = true;
      server.enable = true;
    };

    services = {

      syncthing = {
        syncDir = "${config.customHostSpecificGlobalOptions.pathToDataDirectory}/syncthing";
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
