{ config, ... }:
{
    imports = [
        ../../modules/nixos/security/sops/sops-common.nix
        ../../modules/nixos/security/sops/ssh_development_keys_for_vm.nix
    ];
}
