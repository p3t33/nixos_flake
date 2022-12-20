{ config, lib, pkgs, ... }:

let
    # relative path for wallpaper on system
    # fulle path will be:
    # ~/.config/wallpaper/mountains.jpg
  wallpaperOut = "wallpaper/mountain.jpg";

in

{
  xdg.configFile."${wallpaperOut}".source = ../../wallpaper/mountain.jpg;
}
