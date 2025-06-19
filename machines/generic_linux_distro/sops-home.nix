{ config, pkgs, ... }:
{

  imports = [
    ../../modules/home-manager/secrets/sops-common.nix
    ../../modules/home-manager/secrets/sops-ssh-development-keys-for-vm.nix
    ../../modules/home-manager/secrets/sops-extra-ssh-hosts.nix
    # ../../modules/home-manager/secrets/sops-git-credentials.nix
  ];

  #sops = {
  #  secrets = {

  #    "syncthing/key.pem" = {
  #      path = "${config.customGlobal.syncthing.configDirectory}/key.pem";
  #      mode = "0600";
  #    };

  #    "syncthing/cert.pem" = {
  #      path = "${config.customGlobal.syncthing.configDirectory}/cert.pem";
  #      mode = "0600";
  #    };
  #  };
 # };

  home.packages = with pkgs; [
    age
    sops
  ];

}
