{ ... }:
{
  flake.modules.nixos.networkmanager = {
  networking.networkmanager.enable = true;
};
}
