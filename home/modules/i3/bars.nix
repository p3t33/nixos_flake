{ pkgs,  ... }:


let 
  colors = rec {
    background = "#312f2f";
    background-alt = "#3b4354";

    foreground = "#F1FAEE";

    primary = "#08D9D6";
    secondary = "#047672";
    alert = "#ff2e63";
    disabled = "#707880";

    bg = "#2f343f";
    inactive-bg = "#2f343f";
    text = "#f3f4f5";
    inactive-text = "#676E70";
    urgent-bg = "#E53935";
    
  };
in
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
            background = colors.bg;
            separator = "#757575";

            focusedWorkspace = {
              border = colors.bg;
              background = colors.bg;
              text = colors.text;
            };

            inactiveWorkspace = {
              border = colors.inactive-bg;
              background = colors.inactive-bg;
              text = colors.inactive-text;
            };

            urgentWorkspace = {
              border = colors.urgent-bg;
              background = colors.urgent-bg;
              text = colors.text;
            };

          };
        }

      ];
}
