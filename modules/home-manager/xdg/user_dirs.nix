{ config, lib, ... }:

{
  config = lib.mkIf config.xdg.mimeApps.enable {
    xdg.userDirs = {
      enable = true;
      createDirectories = true;
      
      download = "${config.home.homeDirectory}/Downloads";
      pictures = "${config.home.homeDirectory}/Pictures";
      documents = "${config.home.homeDirectory}/Documents";
      
      # Disable other default directories if you don't want them
      desktop = null;
      publicShare = null;
      templates = null;
      music = null;
      videos = null;
    };
  };
}
