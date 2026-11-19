{ ... }:
{
  flake.modules.nixos.sftp-backup = { config, lib, ... }:
let
  cfg = config.custom.services.sftpBackup;
in
{
  options.custom.services.sftpBackup = {
    enable = lib.mkEnableOption "restricted SFTP receiver for network configuration backups";

    directory = lib.mkOption {
      type = lib.types.strMatching "/([a-zA-Z0-9_-][a-zA-Z0-9._-]*/)*[a-zA-Z0-9_-][a-zA-Z0-9._-]*";
      default = "${config.custom.shared.pathToDataDirectory}/network-backup";
      description = ''
        Root-owned chroot directory. Its ancestors must also be root-owned
        and must not be writable by group or others.
      '';
    };

    authorizedKeys = lib.mkOption {
      type = lib.types.listOf lib.types.singleLineStr;
      default = [];
      description = ''
        Dedicated backup-client public keys. All keys share access to the
        receiver directory. An empty list leaves backup key authentication
        unavailable. Do not use administrator keys or private keys.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    warnings = lib.optional (cfg.authorizedKeys == [])
      "The SFTP backup receiver has no authorized keys; add a dedicated device public key before configuring router backups.";

    # The default SSH PAM profile starts a systemd user manager even for SFTP.
    # Track backup sessions without starting it or launching user services
    # such as Watchman, Mutagen, and tmux for the backup account.
    security.pam.services.sshd-network-backup = {
      unixAuth = false;
      startSession = true;
      rules.session.systemd.settings.class = "user-light";
    };

    services.openssh = {
      enable = true;
      allowSFTP = true;
      extraConfig = ''
        Match User network-backup
            PAMServiceName sshd-network-backup
            ChrootDirectory ${cfg.directory}
            ForceCommand internal-sftp -d / -u 0007
            AuthenticationMethods publickey
            PasswordAuthentication no
            KbdInteractiveAuthentication no
            AuthorizedKeysFile /etc/ssh/authorized_keys.d/%u
            AuthorizedKeysCommand none
            DisableForwarding yes
            PermitTTY no
            PermitTunnel no
            PermitUserRC no
        Match all
      '';
    };

    systemd.tmpfiles.rules = [
      "d ${cfg.directory} 0750 root network-backup -"
      "d ${cfg.directory}/router 2770 network-backup network-backup -"
    ];
  };
};
}
