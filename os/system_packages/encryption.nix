{ config, pkgs, ... }:
{
    environment.systemPackages = with pkgs; [
      #gpg
      gnupg
      # used by gpg-agent as a gui popup
      pinentry_qt


      # general
      cryptomator
      veracrypt

      # U2F libraries - this needs to be tested because not all
      # packages that are installed on Ubuntu 22.04 were installed
      # and found here.
      libfido2 #webauto
      pam_u2f #linux system

      keepass
    ];

}

