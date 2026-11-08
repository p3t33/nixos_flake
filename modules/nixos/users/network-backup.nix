{ config, hostSpecific, lib, pkgs, ... }:
let
  cfg = config.custom.services.sftpBackup;
in
{
  config = lib.mkIf cfg.enable {
    users.users.network-backup = {
      isSystemUser = true;
      description = "Network configuration backup receiver";
      group = "network-backup";
      home = "/";
      createHome = false;
      shell = "${pkgs.shadow}/bin/nologin";
      hashedPassword = "!";
      openssh.authorizedKeys.keys = cfg.authorizedKeys;
    };

    users.groups.network-backup.members = [ hostSpecific.primeUsername ];
  };
}
