{ config, pkgs, ... }:
{

  imports = [ ../../modules/nixos/security/sops/sops-common.nix ];

  sops = {
    secrets = {
      ssh_id_ed25519_vm_key = {
        path = "/home/kmedrish/.ssh/test";
      };

      "syncthing/key.pem" = {
        path = "${config.userDefinedGlobalVariables.syncthingConfigDirectory}/key.pem";
        mode = "0600";
      };

      "syncthing/cert.pem" = {
        path = "${config.userDefinedGlobalVariables.syncthingConfigDirectory}/cert.pem";
        mode = "0600";
      };
    };
  };

  home.packages = with pkgs; [
    age
    sops
  ];

}
