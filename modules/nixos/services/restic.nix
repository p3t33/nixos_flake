{ config, lib, pkgs, ... }:
let
  cfg = config.custom.services.restic;

  pruneOptions = [
    "--keep-daily 7"
    "--keep-weekly 4"
    "--keep-yearly 3"
  ];

  compressionMax = "--compression max";

  # ============================================================
  # SQLite database backups (design and rationale)
  # ============================================================
  #
  # SQLite databases cannot be safely backed up by copying the `.db` file
  # directly while the owning service is running.
  #
  # When SQLite is in WAL (Write-Ahead Logging) mode, active writes are
  # stored in companion runtime files (`.db-wal` and `.db-shm`). Copying
  # the database file or its runtime files while they are being modified
  # can result in incomplete or inconsistent backups.
  #
  # The correct approach is to create a consistent snapshot using
  # `sqlite3 .backup`, and back up that snapshot instead.
  #
  # ------------------------------------------------------------
  # Design goals
  # ------------------------------------------------------------
  #
  # - Always back up a consistent SQLite snapshot
  # - Never back up live SQLite runtime files (`.db`, `-wal`, `-shm`)
  # - Avoid hard-coded paths and duplicated logic
  # - Make adding a new SQLite-backed service a one-line change
  #
  # To achieve this, we declare SQLite databases once as data
  # (`sqliteSnapshotEntries`) and derive everything else from that:
  #
  #   - paths to exclude from restic
  #   - snapshot creation script
  #   - snapshot cleanup script
  #
  # This keeps the configuration "closed for modification and open
  # for extension".
  #

  # ============================================================
  # 1) Declaring SQLite snapshot specifications
  # ============================================================
  #
  # sqliteSnapshotPaths is a helper function that constructs an attribute
  # set describing one SQLite snapshot.
  #
  # It does not perform any I/O. It simply declares:
  #   - src: the live SQLite database file
  #   - dst: the path where a snapshot will be written
  #
  # These records act as the single source of truth for all derived logic.
  sqliteSnapshotPaths = src: dst: { inherit src dst; };

  # sqliteSnapshotEntries is a list of SQLite snapshot specifications.
  #
  # Each entry describes one database that should be snapshotted and backed up.
  # This list is the ONLY place where SQLite databases are declared.
  #
  # All backup scripts and exclude lists are derived from this list using
  # `map` and related functions.
  #
  # To add support for another SQLite-backed service, append a single
  # new entry here.
  sqliteSnapshotEntries =
  []
  ++ lib.optionals config.services.calibre-web.enable (
    let
      calibreDataDir  = config.services.calibre-web.dataDir;
      calibreLibrary = config.services.calibre-web.options.calibreLibrary;
    in
    [
      (sqliteSnapshotPaths "${calibreDataDir}/app.db"      "${calibreDataDir}/app.db.backup")
      (sqliteSnapshotPaths "${calibreDataDir}/gdrive.db"   "${calibreDataDir}/gdrive.db.backup")
      (sqliteSnapshotPaths "${calibreLibrary}/metadata.db" "${calibreLibrary}/metadata.db.backup")
    ]
    );

  # ============================================================
  # 2) Excluding live SQLite runtime files
  # ============================================================
  #
  # For each SQLite database source file, SQLite may create additional
  # runtime companion files:
  #
  #   - <db>.db       : main database file
  #   - <db>.db-wal   : write-ahead log containing recent writes
  #   - <db>.db-shm   : shared-memory coordination file
  #
  # These files are actively modified while the service is running and
  # must never be backed up directly.
  #
  # Instead, we exclude all live runtime files from restic and back up
  # only the consistent `.backup` snapshots created via `sqlite3 .backup`.
  #
  # This list is derived automatically from `sqliteSnapshotEntries` and is flattened so it can be
  # passed directly to restic's exclude configuration.
  sqliteEntiresToExclude =
    lib.flatten (map (e: [
      e.src
      "${e.src}-wal"
      "${e.src}-shm"
    ]) sqliteSnapshotEntries);

  # ============================================================
  # 3) Generating snapshot creation and cleanup scripts
  # ============================================================
  #
  # The following values generate shell scripts (as strings) from
  # `sqliteSnapshotEntries`.
  #
  # These scripts are executed by restic hooks:
  #
  # - prepare hook: create SQLite `.backup` snapshots
  # - cleanup hook: remove snapshot files after backup
  #
  # The scripts are generated automatically, so adding or removing
  # databases requires no changes here.

  # ------------------------------------------------------------
  # Snapshot creation script
  # ------------------------------------------------------------
  #
  # For each SQLite entry:
  #   - verify the source database exists
  #   - create a consistent snapshot using `sqlite3 .backup`
  #   - echo is added for journalctl.
  sqliteBackupScript =
    lib.concatStringsSep "\n" (map (e: ''
      if [ -f "${e.src}" ]; then
        echo "SQLite snapshot: ${e.src} -> ${e.dst}"
        ${pkgs.sqlite}/bin/sqlite3 "${e.src}" ".backup '${e.dst}'"
      fi
    '') sqliteSnapshotEntries);

  # ------------------------------------------------------------
  # Snapshot cleanup script
  # ------------------------------------------------------------
  #
  # After the backup completes, remove the generated `.backup` files.
  #
  # Cleanup is not strictly required for correctness (snapshots are
  # overwritten on the next run), but it avoids leaving extra copies
  # of databases on disk.
  sqliteCleanupScript =
  lib.concatStringsSep "\n" (map (e: ''
    if [ -e "${e.dst}" ]; then
      echo "Deleting SQLite snapshot: ${e.dst}"
    fi
    rm -f "${e.dst}"
  '') sqliteSnapshotEntries);


  # ============================================================
  # 4) Hook wiring
  # ============================================================
  #
  # Hooks are only enabled if at least one SQLite snapshot entry exists.
  #
  # This ensures that:
  # - no shell code is generated when SQLite is not in use
  # - restic runs without unnecessary hooks
  sqlitePrepareHook =
    lib.optionalString (sqliteSnapshotEntries != []) ''
      set -euo pipefail
      ${sqliteBackupScript}
    '';

  sqliteCleanupHook =
    lib.optionalString (sqliteSnapshotEntries != []) ''
      set -euo pipefail
      ${sqliteCleanupScript}
    '';

  # --------------------------------------------------------
  backupPaths =
    []
    ++ lib.optionals config.services.syncthing.enable [
      config.custom.services.syncthing.syncDir
    ]
    ++ lib.optionals config.services.immich.enable [
      config.services.immich.mediaLocation
    ]
    ++ lib.optionals config.services.paperless.enable [
      config.services.paperless.exporter.directory
    ]
    ++ lib.optionals config.services.calibre-web.enable [
      # Calibre library (books + metadata.db)
      config.services.calibre-web.options.calibreLibrary
      # Calibre-Web app state (app.db, gdrive.db)
      config.services.calibre-web.dataDir
    ];

  backupExclude =
    []
    ++ lib.optionals config.services.syncthing.enable [
      # for syncthing versioning
      "**/.stversions/**"
    ]
    ++ lib.optionals config.services.immich.enable [
      "${config.services.immich.mediaLocation}/thumbs/**"
      "${config.services.immich.mediaLocation}/encoded-video/**"
    ]
    ++ sqliteEntiresToExclude;
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
        listenAddress = "${config.customGlobal.anyIPv4}:9005"; # Expose Restic API & metrics
        prometheus = true; # Enable Prometheus metrics
        appendOnly = true; # Ensure backups are append-only for safty from being hacked.
      };

      backups = {

        # name of restic repository, should be descriptive but could be anything.
        local = {
          # will create restic repo if it does not exit yet.
          initialize = true;
          # for syncthing versioning

          exclude =
            []
            ++ backupExclude;

          # A file with restic repository path.
          repositoryFile = config.sops.secrets."restic/local/repositoryPathFile".path;
          passwordFile = config.sops.secrets."restic/local/passwordFile".path;

          # Paths to backup.
          paths =
            []
            ++ backupPaths;

          pruneOpts =
            []
            ++ pruneOptions;

          extraBackupArgs = [ compressionMax ];
          backupPrepareCommand = sqlitePrepareHook;
          backupCleanupCommand = sqliteCleanupHook;

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
          exclude =
            []
            ++ backupExclude;

          repositoryFile = config.sops.secrets."restic/gdrive/repositoryPathFile".path;
          rcloneConfigFile = config.sops.secrets."restic/gdrive/rcloneConfigFile".path;
          passwordFile = config.sops.secrets."restic/gdrive/passwordFile".path; # <-- clearly points here

          paths =
            [
              "${config.customHostSpecificGlobalOptions.pathToDataDirectory}/pictures"
            ]
            ++ backupPaths;

          pruneOpts =
            []
            ++ pruneOptions;

          extraBackupArgs = [ compressionMax ];
          backupPrepareCommand = sqlitePrepareHook;
          backupCleanupCommand = sqliteCleanupHook;

          timerConfig = {
            OnCalendar = "02:05";
            RandomizedDelaySec = "1h";
          };

        };

        amazon = {
          initialize = true;
          exclude =
            []
            ++ backupExclude;

          repositoryFile = config.sops.secrets."restic/amazon/repositoryPathFile".path;
          rcloneConfigFile = config.sops.secrets."restic/amazon/rcloneConfigFile".path;
          passwordFile = config.sops.secrets."restic/amazon/passwordFile".path;

          paths =
            [
              "${config.customHostSpecificGlobalOptions.pathToDataDirectory}/pictures"
            ]
            ++ backupPaths;

          pruneOpts =
            []
            ++ pruneOptions;

          extraBackupArgs = [ compressionMax ];
          backupPrepareCommand = sqlitePrepareHook;
          backupCleanupCommand = sqliteCleanupHook;

          timerConfig = {
            OnCalendar = "04:05";
            RandomizedDelaySec = "1h";
          };
        };
      };
    };
  };
}
