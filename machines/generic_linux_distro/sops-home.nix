{ config, pkgs, inputs, ... }:
{

  imports = [
    inputs.self.modules.homeManager.sops-common
    inputs.self.modules.homeManager.sops-ssh-development-keys-for-vm
    inputs.self.modules.homeManager.sops-extra-ssh-hosts
    # inputs.self.modules.homeManager.sops-git-credentials
  ];

  #sops = {
  #  secrets = {

  #    "syncthing/key.pem" = {
  #      path = "${config.custom.shared.syncthing.configDirectory}/key.pem";
  #      mode = "0600";
  #    };

  #    "syncthing/cert.pem" = {
  #      path = "${config.custom.shared.syncthing.configDirectory}/cert.pem";
  #      mode = "0600";
  #    };
  #  };
 # };

  home.packages = with pkgs; [
    age
    sops
  ];

}
