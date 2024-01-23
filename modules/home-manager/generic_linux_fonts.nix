# This file is only to be used for on a stand alone
# installation of home-manger.

{ pkgs, ... }:
{
  home.packages = with pkgs; [
      nerdfonts
      powerline-fonts
      font-awesome
  ];

  # Required to autoload fonts from packages for the system to use.
  fonts.fontconfig.enable = true;
}
