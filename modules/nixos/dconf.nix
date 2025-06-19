{ config, lib, ... }:
{

  config = lib.mkIf config.programs.dconf.enable {
    # used as a to access:
    # - gnome or gnome based tools on NixOS
    # - GTK-based applications that rely on DConf for storing configuration.
    programs.dconf = {
      packages = [];
      profiles = {};
    };
  };
}
