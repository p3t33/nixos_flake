{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.custom.apps.desktopEnvironment;
in
{
  options.custom.apps.desktopEnvironment.enable =
    lib.mkEnableOption "Enable desktop environment packages";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      rofi-power-menu
      sqlite
      libreoffice

      # Sound control
      pavucontrol
    ];
  };
}
