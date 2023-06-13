{config, pkgs, ...}:

{

  services.xserver = {
    enable = true;
    logFile = "/var/log/Xorg.0.log"; # Enables logging to this file
    layout = "us,il";
    xkbVariant = "";
    xkbOptions = "grp:win_space_toggle";

    displayManager = {
      lightdm.enable = true;
      defaultSession = "none+i3";
      autoLogin = {
        enable = true;
        user = "kmedrish";
      };
    };

    desktopManager = {
      cinnamon.enable = true;
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
        glxinfo # query the properties of an OpenGL implementation
      ];
    };
  };

}
