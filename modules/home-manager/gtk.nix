{ pkgs, config, ... }:
{
  home.packages = with pkgs; [
    arc-theme
    arc-icon-theme
    papirus-icon-theme
    tela-icon-theme
  ];

  gtk = {
    enable = true;
    font = {
      name = config.customGlobalOptions.font.sansSerif;
      size = 12;
    };

    theme = {
      name = "Nordic-darker";
      package = pkgs.nordic;
    };

    iconTheme = {
      name = "Nordic-bluish";
      package = pkgs.nordic;
    };

    cursorTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
  };
}
