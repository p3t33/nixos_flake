{ pkgs, ... }:
{
    environment.systemPackages = with pkgs; [
      # gpg
      # ---
      gnupg
      pinentry_qt # used by gpg-agent as a gui popup


      # Whole disk partition and container on local device encryption.
      veracrypt

      # Backup of local files to the cloud.
      cryptomator
      gocryptfs

      # FIDO/FIDO(U2F) libraries
      libfido2 # webauto
      pam_u2f #linux(sudo, display manager, console login...)

      keepass # Local password manage
    ];

}

