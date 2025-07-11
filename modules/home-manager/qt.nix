{ config, lib, pkgs, ... }:

let
  cfg = config.customOptions.enableModule.qt;
in
{
  options.customOptions.enableModule.qt = lib.mkEnableOption "Enable Qt theming and Kvantum config";

  config = lib.mkIf cfg {
    home.packages = with pkgs; [
      nordic
    ];

    qt = {
      enable = true;
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

