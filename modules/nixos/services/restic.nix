{ config, ... }:
let
  pruneDaily = "--keep-daily 7";
  pruneWeekly = "--keep-weekly 4";
  pruneYearly = "--keep-yearly 3";
  compressionMax = "--compression max";
in
{

  sops.secrets."restic/local/repositoryPathFile" = { };
  sops.secrets."restic/local/passwordFile" = { };
  sops.secrets."restic/gdrive/repositoryPathFile" = { };
  sops.secrets."restic/gdrive/passwordFile" = { };
  sops.secrets."restic/gdrive/rcloneConfigFile" = { };

  services.restic.backups = {
    # name of restic repository, should be descriptive but could be anything.
    local = {
      # will create restic repo if it does not exit yet.
      initialize = true;

      # A file with restic repository path.
      repositoryFile = config.sops.secrets."restic/local/repositoryPathFile".path;
      passwordFile = config.sops.secrets."restic/local/passwordFile".path;

      # Paths to backup.
      paths = [ "${config.userDefinedGlobalVariables.pathToDataDirectory}/Sync" ];

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
        RandomizedDelaySec = "2h";
      };
    };

    gdrive = {
      initialize = true;
      repositoryFile = config.sops.secrets."restic/gdrive/repositoryPathFile".path;
      rcloneConfigFile = config.sops.secrets."restic/gdrive/rcloneConfigFile".path;
      passwordFile = config.sops.secrets."restic/gdrive/passwordFile".path; # <-- clearly points here

      paths = [ "${config.userDefinedGlobalVariables.pathToDataDirectory}/Sync" ];
      pruneOpts = [
        pruneDaily
        pruneWeekly
        pruneYearly
      ];

      extraBackupArgs = [ compressionMax ];

      timerConfig = {
          OnCalendar = "04:00";
          RandomizedDelaySec = "2h";
      };

    };
  };

}
