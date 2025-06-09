{ config, ... }:
{
services.postgresqlBackup = {
  enable = true;
  # Store backups in a dedicated directory.
  location = "/var/backup/postgresql";
  # Automatically back up all databases managed by NixOS.
  databases = config.services.postgresql.ensureDatabases;
  # Schedule the backup to run daily.
  startAt = "03:00:00"; # Runs daily at 3 AM sharp
  # Use zstd compression for the dump files.
  compression = "zstd";
};
}

