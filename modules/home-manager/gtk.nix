{ pkgs, config, ... }:
{
  home.packages = with pkgs; [ adwaita-icon-theme ];

  gtk = {
    enable = true;
    font.name = config.userDefinedGlobalVariables.font.sansSerif;
    iconTheme.name = "Adwaita";
    theme.name = "Adwaita-dark";
  };
}
