{ config, lib, pkgs, ... }:
{

  config = lib.mkIf config.qt.enable {
    home.packages = with pkgs; [
      nordic
    ];

    qt = {
      platformTheme.name = "gtk";

      style = {
        name = "kvantum";
        package = pkgs.libsForQt5.qtstyleplugin-kvantum;
      };
      # Doens not require to setup xdg.configFile."Kvantum/kvantum.kvconfig".text = ''
      # style = {
      #   name = "adwaita-dark";
      #   package = pkgs.adwaita-qt;
      # };
    };

    xdg.configFile."Kvantum/kvantum.kvconfig".text = ''
      [General]
      theme=Nordic-Darker
    '';
  };
}

