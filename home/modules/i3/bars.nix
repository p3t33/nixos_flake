{ pkgs,  ... }:


{

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
            background = config.userDefinedGlobalVariables.colors.bg;
            separator = "#757575";

            focusedWorkspace = {
              border = config.userDefinedGlobalVariables.colors.bg;
              background = config.userDefinedGlobalVariables.colors.bg;
              text = config.userDefinedGlobalVariables.colors.text;
            };

            inactiveWorkspace = {
              border = config.userDefinedGlobalVariables.colors.inactive-bg;
              background = config.userDefinedGlobalVariables.colors.inactive-bg;
              text = config.userDefinedGlobalVariables.colors.inactive-text;
            };

            urgentWorkspace = {
              border = config.userDefinedGlobalVariables.colors.urgent-bg;
              background = config.userDefinedGlobalVariables.colors.urgent-bg;
              text = config.userDefinedGlobalVariables.colors.text;
            };

          };
        }

      ];
}
