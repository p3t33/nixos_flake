{ config, lib, pkgs, ... }:

{

  config = lib.mkIf config.gtk.enable {
    home.packages = with pkgs; [
      arc-theme
      arc-icon-theme
      # papirus-icon-theme
      # tela-icon-theme
    ];

    gtk = {
      font = {
        name = config.customGlobal.font.sansSerif;
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
  };
}
