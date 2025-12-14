{ config, lib, ... }:
let
cfg = config.custom.services.restic;

  pruneDaily = "--keep-daily 7";
  pruneWeekly = "--keep-weekly 4";
  pruneYearly = "--keep-yearly 3";
  compressionMax = "--compression max";
in
{
  options.custom.services.restic.enable = lib.mkEnableOption "Enable Restic backups and Restic REST server";

  config = lib.mkIf cfg.enable {
    sops.secrets."restic/local/repositoryPathFile" = { };
    sops.secrets."restic/local/passwordFile" = { };
    sops.secrets."restic/gdrive/repositoryPathFile" = { };
    sops.secrets."restic/gdrive/passwordFile" = { };
    sops.secrets."restic/gdrive/rcloneConfigFile" = { };
    sops.secrets."restic/amazon/repositoryPathFile" = { };
    sops.secrets."restic/amazon/passwordFile" = { };
    sops.secrets."restic/amazon/rcloneConfigFile" = { };

    services.restic = {

      server = {
        enable = true;
        extraFlags = [ "--no-auth" ];
        listenAddress = "0.0.0.0:9005"; # Expose Restic API & metrics
        prometheus = true; # Enable Prometheus metrics
        appendOnly = true; # Ensure backups are append-only for safty from being hacked.
      };

      backups = {

        # name of restic repository, should be descriptive but could be anything.
        local = {
          # will create restic repo if it does not exit yet.
          initialize = true;
          # for syncthing versioning
          exclude = [
            "**/.stversions/**"
            "${config.services.immich.mediaLocation}/thumbs/**"
            "${config.services.immich.mediaLocation}/encoded-video/**"
          ];

          # A file with restic repository path.
          repositoryFile = config.sops.secrets."restic/local/repositoryPathFile".path;
          passwordFile = config.sops.secrets."restic/local/passwordFile".path;

          # Paths to backup.
          paths = [
            "${config.custom.services.syncthing.syncDir}"
            "${config.services.immich.mediaLocation}"
          ];

          pruneOpts = [
            pruneDaily
            pruneWeekly
            pruneYearly
          ];

          extraBackupArgs = [ compressionMax ];

          timerConfig = {
            # backup will happen each day  between 00:05 and 02:05.
            # The exact start time within this window will vary each day
            # due to the randomized delay.
            OnCalendar = "00:05";
            RandomizedDelaySec = "1h";
          };
        };

        gdrive = {
          initialize = true;
          # for syncthing versioning
          exclude = [
            "**/.stversions/**"
            "${config.services.immich.mediaLocation}/thumbs/**"
            "${config.services.immich.mediaLocation}/encoded-video/**"
          ];

          repositoryFile = config.sops.secrets."restic/gdrive/repositoryPathFile".path;
          rcloneConfigFile = config.sops.secrets."restic/gdrive/rcloneConfigFile".path;
          passwordFile = config.sops.secrets."restic/gdrive/passwordFile".path; # <-- clearly points here

          paths = [
            "${config.custom.services.syncthing.syncDir}"
            "${config.customHostSpecificGlobalOptions.pathToDataDirectory}/pictures"
            "${config.services.immich.mediaLocation}"
          ];
          pruneOpts = [
            pruneDaily
            pruneWeekly
            pruneYearly
          ];

          extraBackupArgs = [ compressionMax ];

          timerConfig = {
            OnCalendar = "02:05";
            RandomizedDelaySec = "1h";
          };

        };

        amazon = {
          initialize = true;
          # for syncthing versioning
          exclude = [
            "**/.stversions/**"
            "${config.services.immich.mediaLocation}/thumbs/**"
            "${config.services.immich.mediaLocation}/encoded-video/**"
          ];

          repositoryFile = config.sops.secrets."restic/amazon/repositoryPathFile".path;
          rcloneConfigFile = config.sops.secrets."restic/amazon/rcloneConfigFile".path;
          passwordFile = config.sops.secrets."restic/amazon/passwordFile".path;

          paths = [
            "${config.custom.services.syncthing.syncDir}"
            "${config.customHostSpecificGlobalOptions.pathToDataDirectory}/pictures"
            "${config.services.immich.mediaLocation}"
          ];

          pruneOpts = [
            pruneDaily
            pruneWeekly
            pruneYearly
          ];

          extraBackupArgs = [ compressionMax ];

          timerConfig = {
            OnCalendar = "04:05";
            RandomizedDelaySec = "1h";
          };
        };
      };
    };
  };
}
