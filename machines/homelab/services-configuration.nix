{ config, ... }:
{
  imports = [
    ../../modules/nixos/services # imported via default.nix
  ];

  customOptions = {
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

      devicesToShareTaskWarriorFolderWith = [
        "${config.customOptions.syncthing.deviceNames.work-pc}"
        "${config.customOptions.syncthing.deviceNames.home-desktop}"
       ];

      devicesToShareDevResourcesFolderWith = [
        "${config.customOptions.syncthing.deviceNames.work-pc}"
        "${config.customOptions.syncthing.deviceNames.home-desktop}"
      ];

      devicesToShareDatabaseFolderWith = [
        "${config.customOptions.syncthing.deviceNames.home-desktop}"
      ];

      devicesToShareDocumentsFolderWith = [
        "${config.customOptions.syncthing.deviceNames.home-desktop}"
      ];

      devicesToShareStudyFolderWith = [
        "${config.customOptions.syncthing.deviceNames.home-desktop}"
      ];

      enableRemoteDevice = {
        home-desktop = true;
        work-pc = true;
      };

      enableFolder = {
        taskwarrior = true;
        devResources = true;
        database = true;
        documents = true;
        study = true;
      };
    };

    enableServicesProfile = {
      core = true;
      server = true;
    };

    enableModule = {
      syncthing = true;
      nginx = true;
      adguardHome = true;
      restic = true;
      homepageDashboard = true;
      calibre-web = true;
      jellyfin = true;
      prowlarr = true;
      jackett = true;
      sonarr = true;
      radarr = true;
      readarr = true;
      bazarr = true;
      deluge = true;
      sabnzbd = true;
      wireguard-server = true;
      inadyn = true;
      gatus = true;
      prometheus = true;
      promtail = true;
      loki = true;
      grafana = true;
      samba = true;
      paperless = true;
      postgresql = true;
      postgresqlBackup = true;
    };
  };
}
