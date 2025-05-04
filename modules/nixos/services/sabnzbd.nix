{config, ...}:
{
  services.sabnzbd = {
    enable = true;
    group = config.userDefinedGlobalVariables.mediaGroup;
    # configFile = "/var/lib/sabnzbd/sabnzbd.ini"; # will be created if not exist
    openFirewall = true;     # expose web interface port (8080 by default)
  };

    systemd.tmpfiles.rules = [
      "d ${config.userDefinedGlobalVariables.pathToUsenetDirectory} 0770 ${config.userDefinedGlobalVariables.primeUsername} ${config.userDefinedGlobalVariables.mediaGroup} -"
      "d ${config.userDefinedGlobalVariables.pathToUsenetDirectory}/complete 0770 ${config.services.sabnzbd.user} ${config.userDefinedGlobalVariables.mediaGroup} -"
      "d ${config.userDefinedGlobalVariables.pathToUsenetDirectory}/incomplete 0770 ${config.services.sabnzbd.user} ${config.userDefinedGlobalVariables.mediaGroup} -"
    ];

}
