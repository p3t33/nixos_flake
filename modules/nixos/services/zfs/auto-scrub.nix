{ config, lib, ... }:
{
  config = lib.mkIf config.services.zfs.autoScrub.enable {
    services.zfs.autoScrub = {
      interval = "monthly";
    };
  };
}
