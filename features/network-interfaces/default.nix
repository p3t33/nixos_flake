{ ... }:
{
  flake.modules.nixos.network-interfaces = import ./_/default.nix;
}
