{ config, lib, pkgs, ... }:

let
  cfg = config.custom.apps.encryption;
in
{
  options.custom.apps.encryption.enable = lib.mkEnableOption "Enable encryption tools and libraries";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # gpg
      # ---
      gnupg
      pinentry-qt # used by gpg-agent as a gui popup

      # Whole disk partition and container on local device encryption.
      veracrypt

      # Backup of local files to the cloud.
      #cryptomator # is broken at the time of this commit(updating to 24.11).
      gocryptfs
      sops
      age
      ssh-to-age

      # argon2 support
      libargon2

      # FIDO/FIDO(U2F) libraries
      libfido2 # webauto
      pam_u2f # linux(sudo, display manager, console login...)

      keepass # Local password manage
    ];
  };
}
