{config, lib, hostSpecific, ...}:
let
  pathToUsenetDirectory = "${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/usenet";
in
{
  options.custom = {
    servicePort.sabnzbd = lib.mkOption {
      type = lib.types.int;
      default = 8080;
      description = "sabnzbd port";
    };
  };

  config = lib.mkIf config.services.sabnzbd.enable {
    services.sabnzbd = {
      group = config.customGlobal.mediaGroup;
      # configFile = "/var/lib/sabnzbd/sabnzbd.ini"; # will be created if not exist
      openFirewall = true;     # expose web interface port (8080 by default)
    };

    systemd.tmpfiles.rules = [
      "d ${pathToUsenetDirectory} 0770 ${hostSpecific.primeUsername} ${config.customGlobal.mediaGroup} -"
      "d ${pathToUsenetDirectory}/complete 0770 ${config.services.sabnzbd.user} ${config.customGlobal.mediaGroup} -"
      "d ${pathToUsenetDirectory}/incomplete 0770 ${config.services.sabnzbd.user} ${config.customGlobal.mediaGroup} -"
    ];
  };
}
