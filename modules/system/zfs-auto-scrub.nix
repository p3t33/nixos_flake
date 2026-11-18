{ ... }:
{
  flake.modules.nixos.zfs-auto-scrub = { config, lib, ... }:
{
  config = lib.mkIf config.services.zfs.autoScrub.enable {
    services.zfs.autoScrub = {
      interval = "monthly";
    };
  };
};
}
