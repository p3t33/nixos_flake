{ config, lib, pkgs, ... }:

let
  cfg = config.customOptions.enableModule.i3Bar;
in
{

  options.customOptions.enableModule.i3Bar = lib.mkEnableOption "Enable Starship prompt configuration";

  config = lib.mkIf cfg {
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
      statusCommand = "${pkgs.i3status}/bin/i3status";
      colors = {
        background = config.customGlobalOptions.colors.bg;
        separator = "#757575";

        focusedWorkspace = {
          border = config.customGlobalOptions.colors.bg;
          background = config.customGlobalOptions.colors.bg;
          text = config.customGlobalOptions.colors.text;
        };

        inactiveWorkspace = {
          border = config.customGlobalOptions.colors.inactive-bg;
          background = config.customGlobalOptions.colors.inactive-bg;
          text = config.customGlobalOptions.colors.inactive-text;
        };

        urgentWorkspace = {
          border = config.customGlobalOptions.colors.urgent-bg;
          background = config.customGlobalOptions.colors.urgent-bg;
          text = config.customGlobalOptions.colors.text;
        };

      };
      }
    ];
  };
}
