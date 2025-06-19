{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.zfsAutoScrub;
in
{
  options.customOptions.enableModule.zfsAutoScrub = lib.mkEnableOption "Enable ZFS auto scrub service";

  config = lib.mkIf cfg {
    services.zfs.autoScrub = {
      enable = true;
      interval = "monthly";
    };
  };
}
