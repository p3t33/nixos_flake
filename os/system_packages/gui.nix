{ config, pkgs, ... }:
{
    environment.systemPackages = with pkgs; [

      # web browser
      firefox
      google-chrome


      # graphics
      gimp
      vlc
      flameshot

      # note taking
      cherrytree

      gnome.nautilus

      # terminal
      alacritty

      sxhkd
      gparted

      gpick # color picker.
    ];

}


