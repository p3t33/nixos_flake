{ config, lib, pkgs, ... }:

let
  cfg = config.custom.apps.gui;
in
{
  options.custom.apps.gui.enable = lib.mkEnableOption "Enable GUI utilities and graphical system tools";

  config = lib.mkIf cfg.enable {
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

      # Media
      obs-studio # video recorder / live streming.
      audacity # audio editing
      kdePackages.kdenlive # video editing.

      # Remote control
      tigervnc
      nomachine-client

      # pdf annotation
      xournalpp
      # pdf viewer
      zathura

      # diagram
      drawio

      # comics readar
      foliate
      mcomix
    ];
  };
}
