{config, pkgs, ...}:

{

  services.xserver = {
    enable = true;
    layout = "us,il";
    xkbVariant = "";
    xkbOptions = "grp:win_space_toggle";

    displayManager = {
      lightdm.enable = true;
      defaultSession = "none+i3";
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
      ];
    };
  };

}
