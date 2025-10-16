{ pkgs, config, lib, hostSpecific, ... }:
{

  config = lib.mkIf config.services.samba.enable {
    sops.secrets.smbpasswd = {};

    # creates smbd.service
    services.samba = {
      smbd.enable = true; # optional, this is true by default
      nmbd.enable = false;
      winbindd.enable = false;
      openFirewall = true;

      settings = {
        # [global] section in a traditional smb.conf.
        global = {
            security = "user";
            "map to guest" = "Bad User";
            "socket options" = "TCP_NODELAY";
        };

        # [private] section in a traditional smb.conf.
        private = {
            path = "${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/torrents";
            browseable = "yes";
            "guest ok" = "no";
            "valid users" = "${hostSpecific.primeUsername}";
            "read only" = "yes";
        };
      };
    };

    # Activation scripts are executed after every `nixos-rebuild`. The "setupSecrets"
    # script is automatically managed by sops-nix and is responsible for decrypting and placing
    # secrets into /run/secrets before dependent scripts run.
    #
    # This activation script runs after setupSecrets has completed, ensuring secrets are ready.
    #
    # The reason this script is required is because currently there is no declarative way
    # to define Samba users through the standard `services.samba` module. Without importing
    # a Samba user into Samba's internal database, the Samba service is not fully functional.
    #
    # Important notes:
    # - The Samba user must exactly match an existing Linux system username and have the same UID (e.g., 1000).
    #
    # How to prepare the Samba user credentials:
    #
    # 1. Manually create a Samba user and set a password (you can use a VM or temporary system):
    #    sudo smbpasswd -a <existing-system-username>
    #
    # 2. Export the credentials to a file in smbpasswd format:
    #    sudo pdbedit -L -w | grep '^<username>:' > samba-<username>.smbpasswd
    #
    # 3. Ensure the exported file looks like this:
    #    <username>:<uid>:XXXXXXXXXXXXXXXXXXXXXXXX:YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY:[U          ]
    #
    #    Make sure the UID in the file matches the actual UID on the NixOS system.
    #
    # 4. Encrypt this smbpasswd line using sops-nix and add it as a secret in your Nix configuration.
    #
    # The following activation script then automatically imports these credentials into Samba's database.
    systemd.services.sambaUserSetup = {
      description = "Import Samba user from sops secret into Samba database";
      after = [ "smbd.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "samba-user-setup" ''
          set -euo pipefail

          echo "Checking for existing Samba user: ${hostSpecific.primeUsername}"
          if ! ${pkgs.samba}/bin/pdbedit -L | grep -q '^${hostSpecific.primeUsername}:'; then
            echo "Importing Samba user ${hostSpecific.primeUsername}..."
            ${pkgs.samba}/bin/pdbedit \
              -i smbpasswd:${config.sops.secrets.smbpasswd.path} \
              -e tdbsam:/var/lib/samba/private/passdb.tdb
            echo "Samba user ${hostSpecific.primeUsername} imported successfully."
          else
            echo "Samba user ${hostSpecific.primeUsername} already exists, skipping."
          fi
        '';
      };
    };
  };

  # Testing Samba Configuration
  #
  # After applying your configuration(and using systemctl to make sure smbd is running):
  #
  # 1. Connect from another Linux machine using smbclient:
  #
  #    smbclient //<ip of the server where smbd is running>/private -U <username>
  #
  #    After entering your password, you should see:
  #
  #      smb: \>
  #
  #    List files to verify access:
  #
  #      smb: \> ls
  #
  # 2. Verify active connections on your NixOS Samba server:
  #
  #    sudo smbstatus -S
  #
  #    Successful connection output example:
  #
  #    Service      pid     Machine       Connected at
  #    ------------------------------------------------------------
  #    private      1234    <ip of connected device> Fri Mar 28 00:28:44 2025
}
