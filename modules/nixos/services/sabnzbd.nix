{config, lib, hostSpecific, ...}:
let
  pathToUsenetDirectory = "${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/usenet";
in
{
  options.customOptions.servicePort.sabnzbd = lib.mkOption {
    type = lib.types.int;
    default = 8080;
    description = "sabnzbd port";
  };

  config = {
    services.sabnzbd = {
      enable = true;
      group = config.customGlobalOptions.mediaGroup;
      # configFile = "/var/lib/sabnzbd/sabnzbd.ini"; # will be created if not exist
      openFirewall = true;     # expose web interface port (8080 by default)
    };

    systemd.tmpfiles.rules = [
      "d ${pathToUsenetDirectory} 0770 ${hostSpecific.primeUsername} ${config.customGlobalOptions.mediaGroup} -"
      "d ${pathToUsenetDirectory}/complete 0770 ${config.services.sabnzbd.user} ${config.customGlobalOptions.mediaGroup} -"
      "d ${pathToUsenetDirectory}/incomplete 0770 ${config.services.sabnzbd.user} ${config.customGlobalOptions.mediaGroup} -"
    ];
  };
}
