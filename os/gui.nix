{ pkgs, ... }:
{
    environment.systemPackages = with pkgs; [
    rofi-power-menu
    polybar
    nitrogen # A wallpaper browser and setter for X11
    picom # A fork of XCompMgr, a sample compositing manager for X servers
    sqlite # rofi-firefox-bookmarks needs this package as a dependency.
    libreoffice
  ];

}
