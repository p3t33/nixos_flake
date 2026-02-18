{ config, lib, ... }:
{
  config = lib.mkIf config.services.immich.enable {

    services.immich = {

      # Where immich web gui listens
      host = config.customGlobal.anyIPv4;
      port = 2283;
      openFirewall = true;

      # Where photos/videos are stored
      mediaLocation = "${config.customGlobal.pathToMediaDirectory}/immich";

      # Creates postgres database.
      #
      # Although I have a separate module for postgress that creates databases and users and takes
      # care of ownership. And although allowing the immich unit to define postgress configuration
      # goes against single responsibility it still makes a lot of sense to use the immich logic
      # for postgress as the alternative is to duplicate code from the immich module to my postgress
      # module and then keep them in sync when invariably things will change in the immich module.
      database = {
        enable = true;
        # uses unix socket connection
        host = "/run/postgresql";
        port = 5432;
        user = "immich";
        name = "immich";
        createDB = true;
      };

      redis.enable = true;

      # Face recognition, object detection, etc.
      machine-learning.enable = true;
    };

    # makes sure directory for immichmedia exist and immich the has the rights to it.
    systemd.tmpfiles.rules = [
      "d ${config.services.immich.mediaLocation} 0750 ${config.services.immich.user} ${config.services.immich.group} - -"
    ];
  };
}


