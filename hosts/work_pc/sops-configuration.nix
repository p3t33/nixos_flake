{ config, ... }:
{
    imports = [
        ../../modules/nixos/security/sops/sops-common.nix
        ../../modules/nixos/security/sops/sops-syncthing.nix
        ../../modules/nixos/security/sops/sops-extra-ssh-hosts.nix
        ../../modules/nixos/security/sops/sops-git-credentials.nix
        ../../modules/nixos/security/sops/ssh_development_keys_for_vm.nix
    ];
}
