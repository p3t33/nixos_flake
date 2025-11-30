{ config, pkgs, lib, ... }:
{

  config = lib.mkIf config.services.xserver.enable {
    services.xserver = {
      logFile = "/var/log/Xorg.0.log"; # Enables logging to this file
      xkb = {
        layout = "us,il";
        variant = "";
        options = "grp:caps_toggle";
      };

      displayManager = {
        lightdm.enable = true;
      };

      desktopManager = {
        xterm.enable = false;
      };

      windowManager.i3 = {
        enable = true;
        #configFile = "/etc/i3.conf";
        extraPackages = with pkgs; [
          rofi
          i3status
          i3blocks

          # Can query the window manager for information and is dependency for
          # the rofi Firefox bookmars script.
          wmctrl

          # Monitor control
          arandr
          xorg.xrandr

          # debugging and information
          xorg.xdpyinfo
          mesa-demos # query the properties of an OpenGL implementation
        ];
      };
    };
  };
}
