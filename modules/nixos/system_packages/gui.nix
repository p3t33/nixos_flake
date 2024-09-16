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

      # terminal
      alacritty

      sxhkd
      gparted

      gpick # color picker.
      pcmanfm

      gpt4all # generative AI packages.

      # Media
      obs-studio # video recorder / live streming.
      audacity # audio editing
      libsForQt5.kdenlive # video editing.

      # Remote control
      tigervnc
      nomachine-client
    ];

}


