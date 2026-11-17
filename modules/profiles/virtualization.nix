{ ... }:
{
  flake.modules.nixos.profile-system = { config, lib, ... }:
    let g = config.custom.profiles.system; in
    {
      # virtualbox is not enbled by default as it is mostly used for cross development.
      config = lib.mkIf g.virtualization.enable {
       virtualisation.docker.enable = true;
        virtualisation.libvirtd.enable = true;
        virtualisation.incus.enable = true; # sets nftables to true.
      };
    };
}
