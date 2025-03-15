{ config, ... }:
{
  services.readarr = {
    enable = true;
    openFirewall = true; # Opens Readarr's port on the firewall (default 8787)
    group = "${config.userDefinedGlobalVariables.mediaGroup}";
    user = "readarr";
  };


  # systemd will create directory on boot(and set ownership and permission) if it doesn't exist yet.
  systemd.tmpfiles.rules = [
    "d ${config.userDefinedGlobalVariables.pathToMediaDirectory}/books 0770 ${config.services.readarr.user} ${config.userDefinedGlobalVariables.mediaGroup} -"
    "d ${config.userDefinedGlobalVariables.pathToMediaDirectory}/audiobooks 0770 ${config.services.readarr.user} ${config.userDefinedGlobalVariables.mediaGroup} -"
  ];

}

