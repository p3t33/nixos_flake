{ config, ... }:
let
  serviceName = "readarr";
in
{
  sops.secrets."${serviceName}/apiKey" = {};

  services.${serviceName} = {
    enable = true;
    openFirewall = true; # Opens Readarr's port on the firewall (default 8787)
    group = "${config.customGlobalOptions.mediaGroup}";
    user = "${serviceName}";
    settings = {
      server = {
        port = 8787;
        urlbase = "/${serviceName}";
      };
    };

    environmentFiles = [
      config.sops.secrets."${serviceName}/apiKey".path
    ];

  };


  # systemd will create directory on boot(and set ownership and permission) if it doesn't exist yet.
  systemd.tmpfiles.rules = [
    "d ${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/books 0770 ${config.services.readarr.user} ${config.customGlobalOptions.mediaGroup} -"
    "d ${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/audiobooks 0770 ${config.services.readarr.user} ${config.customGlobalOptions.mediaGroup} -"
  ];

}

