{config, hostSpecific, ...}:
let
  pathToUsenetDirectory = "${config.userDefinedGlobalVariables.pathToMediaDirectory}/usenet";
in
{
  services.sabnzbd = {
    enable = true;
    group = config.userDefinedGlobalVariables.mediaGroup;
    # configFile = "/var/lib/sabnzbd/sabnzbd.ini"; # will be created if not exist
    openFirewall = true;     # expose web interface port (8080 by default)
  };

    systemd.tmpfiles.rules = [
      "d ${pathToUsenetDirectory} 0770 ${hostSpecific.primeUsername} ${config.userDefinedGlobalVariables.mediaGroup} -"
      "d ${pathToUsenetDirectory}/complete 0770 ${config.services.sabnzbd.user} ${config.userDefinedGlobalVariables.mediaGroup} -"
      "d ${pathToUsenetDirectory}/incomplete 0770 ${config.services.sabnzbd.user} ${config.userDefinedGlobalVariables.mediaGroup} -"
    ];

}
