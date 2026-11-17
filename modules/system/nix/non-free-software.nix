{ ... }:
{
  flake.modules.nixos.non-free-software = { ... }:
{
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
};
}
