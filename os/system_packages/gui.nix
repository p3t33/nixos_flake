{ config, pkgs, ... }:
{
    environment.systemPackages = with pkgs; [

      # web browser
      firefox
      google-chrome


      # graphics
      gimp
      vlc
      arandr
      flameshot




      # note taking
      cherrytree

      gnome.nautilus

      # terminal
      alacritty

      sxhkd
      gparted






    ];

}


