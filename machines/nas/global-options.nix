{ lib, ...}:
{
  imports = [
    ../../modules/nixos/custom-global-options/networking.nix
    ../../modules/nixos/custom-global-options/static-ip.nix
  ];
}
