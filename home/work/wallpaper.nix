{ config, lib, pkgs, ... }:

let
    # relative path for wallpaper on system
    # fulle path will be:
    # ~/.config/wallpaper/city.jpg
  wallpaperOut = "wallpaper/city.jpg";

in

{
  xdg.configFile."${wallpaperOut}".source = ../../wallpaper/city.jpg;
}
