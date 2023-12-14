{ config, ... }:
{
    imports = [
        ../../modules/nixos/security/sops/sops-common.nix
        ../../modules/nixos/security/sops/sops-syncthing.nix
        ../../modules/nixos/security/sops/sops-extra-ssh-hosts.nix
    ];

    sops.secrets.ssh_id_ed25519_vm_key = {
        mode = "0600";
        owner = "${config.userDefinedGlobalVariables.username}";
        path = "${config.userDefinedGlobalVariables.homeDirectory}/.ssh/id_ed25519_vm_key";
    };
}
