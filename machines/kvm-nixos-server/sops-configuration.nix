{ config, ... }:
{
  imports = [
    ../../modules/nixos/security/sops/sops-common.nix
    ../../modules/nixos/security/sops/sops_prime_user_hashed_password.nix
  ];
}
