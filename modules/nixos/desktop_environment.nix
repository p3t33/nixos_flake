{ config, lib, pkgs, ... }:

let
  cfg = config.custom.apps.desktopEnvironment;
in
{
  options.custom.apps.desktopEnvironment.enable = lib.mkEnableOption "Enable desktop environment packages (rofi, polybar, picom, etc.)";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      rofi-power-menu
      polybar
      nitrogen # A wallpaper browser and setter for X11
      picom # A fork of XCompMgr, a sample compositing manager for X servers
      sqlite
      libreoffice

      # Sound control
      pavucontrol
    ];
  };
}
