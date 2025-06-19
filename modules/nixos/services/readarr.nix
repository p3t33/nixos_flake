{ config, lib, ... }:

let
  serviceName = "readarr";
in
{

  config = lib.mkIf config.services.${serviceName}.enable {
    sops.secrets."${serviceName}/apiKey" = {};

    services.${serviceName} = {
      openFirewall = true; # Opens Readarr's port on the firewall (default 8787)
      group = "${config.customGlobal.mediaGroup}";
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
      "d ${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/books 0770 ${config.services.readarr.user} ${config.customGlobal.mediaGroup} -"
      "d ${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/audiobooks 0770 ${config.services.readarr.user} ${config.customGlobal.mediaGroup} -"
    ];
  };
}
