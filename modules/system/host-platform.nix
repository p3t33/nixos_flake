{ ... }:
{
  flake.modules.nixos.host-platform = { lib, ... }:
    {
      nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    };
}
