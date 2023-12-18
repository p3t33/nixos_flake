{ config, ... }:
{
    imports = [
        ../../modules/nixos/security/sops/sops-common.nix
        ../../modules/nixos/security/sops/sops-syncthing.nix
        ../../modules/nixos/security/sops/sops-git-credentials.nix
    ];
}
