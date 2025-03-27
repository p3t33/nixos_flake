{ pkgs, config, ... }:
{
  sops.secrets.smbpasswd= {};

  # creates smbd.service
  services.samba = {
    enable = true;
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

      private = {
          path = "${config.userDefinedGlobalVariables.pathToMediaDirectory}/torrents";
          browseable = "yes";
          "guest ok" = "no";
          "valid users" = "${config.userDefinedGlobalVariables.primeUsername}";
          "read only" = "yes";
      };
    };
  };

  system.activationScripts.sambaUserSetup = {
    # This ensures that the Samba user import script runs only after the SOPS-managed secrets
    # have been decrypted and placed in /run/secrets by the sops-nix module. The "setupSecrets"
    # activation script is automatically registered by sops-nix and is responsible for preparing
    # all declared secrets before any dependent activation scripts run.
    deps = [ "setupSecrets" ];

    text = ''
      if ! ${pkgs.samba}/bin/pdbedit -L | grep -q '^${config.userDefinedGlobalVariables.primeUsername}:'; then
        ${pkgs.samba}/bin/pdbedit \
          -i smbpasswd:${config.sops.secrets.smbpasswd.path} \
          -e tdbsam:/var/lib/samba/private/passdb.tdb
          fi
    '';
  };
}

