{ pkgs,  ... }:
{
  fonts.fonts = with pkgs; [ 
    nerdfonts
    powerline-fonts
    font-awesome
  ];
}
