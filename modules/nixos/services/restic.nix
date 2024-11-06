{ config, ... }:
{

  sops.secrets."restic/local/repositoryPathFile" = { };
  sops.secrets."restic/local/passwordFile" = { };

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

      # Keep 7 daily, 4 weekly, and 3 annual backups.
      pruneOpts = [
        "--keep-daily 7"
        "--keep-weekly 4"
        "--keep-yearly 3"
      ];

      extraBackupArgs = [ "--compression max" ];

      timerConfig = {
        # backup will happen each day ometime between 00:05 and 05:05.
        # The exact start time within this window will vary each day
        # due to the randomized delay.
        OnCalendar = "00:05";
        RandomizedDelaySec = "5h";
      };
    };
  };

}
