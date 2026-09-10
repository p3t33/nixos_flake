{
  config,
  lib,
  pkgs-unstable,
  ...
}:

let
  cfg = config.services.flameshot;
in
{
  config = lib.mkIf cfg.enable {
    services.flameshot = {
      package = pkgs-unstable.flameshot;
      settings.General = {
        disabledTrayIcon = true;
        drawColor = "#2200ff";
        drawThickness = 2;
        savePath = config.xdg.userDirs.pictures;
        showStartupLaunchMessage = false;
      };
    };
  };
}
