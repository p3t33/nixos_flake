{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.dconf;
in
{
  options.customOptions.enableModule.dconf = lib.mkEnableOption "Enable DConf for GNOME/GTK-based configuration support";

  config = lib.mkIf cfg {
    # used as a to access:
    # - gnome or gnome based tools on NixOS
    # - GTK-based applications that rely on DConf for storing configuration.
    programs.dconf.enable = true;
  };
}
