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

    videoDrivers = [ "intel" ];

    windowManager.i3 = {
      enable = true;
      #configFile = "/etc/i3.conf";
      extraPackages = with pkgs; [
        dmenu
	rofi
	i3status
	i3blocks
	#i3lock
      ];
    };
  };
}
