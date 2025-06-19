{ config, lib, pkgs, ... }:

let
  cfg = config.customOptions.enableModule.desktopEnvironment;
in
{
  options.customOptions.enableModule.desktopEnvironment = lib.mkEnableOption "Enable desktop environment packages (rofi, polybar, picom, etc.)";

  config = lib.mkIf cfg {
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
