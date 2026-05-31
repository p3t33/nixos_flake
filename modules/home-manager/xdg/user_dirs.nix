{ config, lib, ... }:

{
  config = lib.mkIf config.xdg.mimeApps.enable {
    xdg.userDirs = {
      enable = true;
      createDirectories = true;
      # Apps read XDG user dirs from ~/.config/user-dirs.dirs directly,
      # exporting them as session env vars is non-standard and redundant.
      setSessionVariables = false;
      
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
