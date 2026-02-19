{ config, lib, pkgs, ... }:

let
  cfg = config.custom.programs.i3Bar;
in
{

  options.custom.programs.i3Bar.enable = lib.mkEnableOption "Enable Starship prompt configuration";

  config = lib.mkIf cfg.enable {
    xsession.windowManager.i3.config.bars = [
    {
      position = "bottom";
      fonts = {
        names = [ "nerdfonts" ];
        size = 20.0;
      };

      # No need to specify path for settings file for i3status bar.
      # By default i3status will look for config files at specific paths.
      # I have a seperate file with definitions for i3status bar and it will
      # generate a config file for i3status to look at.
      statusCommand = "${lib.getExe pkgs.i3status}";
      colors = {
        background = config.custom.shared.colors.bg;
        separator = "#757575";

        focusedWorkspace = {
          border = config.custom.shared.colors.bg;
          background = config.custom.shared.colors.bg;
          text = config.custom.shared.colors.text;
        };

        inactiveWorkspace = {
          border = config.custom.shared.colors.inactive-bg;
          background = config.custom.shared.colors.inactive-bg;
          text = config.custom.shared.colors.inactive-text;
        };

        urgentWorkspace = {
          border = config.custom.shared.colors.urgent-bg;
          background = config.custom.shared.colors.urgent-bg;
          text = config.custom.shared.colors.text;
        };

      };
      }
    ];
  };
}
