{config, lib, hostSpecific, ...}:
let
  pathToUsenetDirectory = "${config.custom.shared.pathToMediaDirectory}/usenet";
in
{
  options.custom.services.sabnzbd = {
    httpPort = lib.mkOption {
      type = lib.types.int;
      default = 8080;
      description = "sabnzbd port";
    };
  };

  config = lib.mkIf config.services.sabnzbd.enable {
    services.sabnzbd = {
      group = config.custom.shared.mediaGroup;
      openFirewall = true;
      configFile = "/var/lib/sabnzbd/sabnzbd.ini";
    };

    systemd.tmpfiles.rules = [
      "d ${pathToUsenetDirectory} 2770 ${config.services.sabnzbd.user} ${config.custom.shared.mediaGroup} -"
      "d ${pathToUsenetDirectory}/complete 2770 ${config.services.sabnzbd.user} ${config.custom.shared.mediaGroup} -"
      "d ${pathToUsenetDirectory}/incomplete 2770 ${config.services.sabnzbd.user} ${config.custom.shared.mediaGroup} -"
    ];


    sops.secrets."sabnzbd/api_key" = { };
    sops.secrets."sabnzbd/nzb_key" = { };
    sops.secrets."sabnzbd/servers/zero/name" = { };
    sops.secrets."sabnzbd/servers/zero/displayname" = { };
    sops.secrets."sabnzbd/servers/zero/host" = { };
    sops.secrets."sabnzbd/servers/zero/username" = { };
    sops.secrets."sabnzbd/servers/zero/password" = { };
    sops.secrets."sabnzbd/servers/zero/port" = { };



    sops.templates."sabnzbd.ini" = {
      path = "/var/lib/sabnzbd/sabnzbd.ini";
      owner = config.services.sabnzbd.user;
      group = config.services.sabnzbd.group;
      mode = "0640";
      restartUnits = [ config.systemd.services.sabnzbd.name ];

      # This is a minimal subset of SABnzbd's full config. Only settings we
      # explicitly own are declared here. SABnzbd fills in all other settings
      # from its own hardcoded defaults when it reads this file on startup.
      content = ''
        __version__ = 19
        __encoding__ = utf-8
        [misc]
        host = "${config.custom.shared.anyIPv4}"
        port = "${toString config.custom.services.sabnzbd.httpPort}"
        url_base = /sabnzbd
        host_whitelist = nas, sabnzbd.nas
        fixed_ports = 1
        api_key = "${config.sops.placeholder."sabnzbd/api_key"}"
        nzb_key = "${config.sops.placeholder."sabnzbd/nzb_key"}"
        download_dir = "${pathToUsenetDirectory}/incomplete"
        complete_dir = "${pathToUsenetDirectory}/complete"
        # admin_dir and log_dir must be absolute paths or SABnzbd resolves them
        # relative to the config file location and fails to write.
        admin_dir = /var/lib/sabnzbd/admin
        log_dir = /var/lib/sabnzbd/logs
        permissions = 775
        cache_limit = 1G
        [servers]
        [["${config.sops.placeholder."sabnzbd/servers/zero/name"}"]]
        name = "${config.sops.placeholder."sabnzbd/servers/zero/name"}"
        displayname = "${config.sops.placeholder."sabnzbd/servers/zero/displayname"}"
        host = "${config.sops.placeholder."sabnzbd/servers/zero/host"}"
        port = ${config.sops.placeholder."sabnzbd/servers/zero/port"}
        username = "${config.sops.placeholder."sabnzbd/servers/zero/username"}"
        password = "${config.sops.placeholder."sabnzbd/servers/zero/password"}"
        connections = 50
        ssl = 1
        ssl_verify = 3
        enable = 1
        [categories]
        [[*]]
        name = *
        order = 0
        pp = 3
        script = None
        priority = 0
        [[movies]]
        name = movies
        order = 1
        priority = -100
        [[tv]]
        name = tv
        order = 2
        priority = -100
        [[audio]]
        name = audio
        order = 3
        priority = -100
        [[software]]
        name = software
        order = 4
        priority = -100
        [[books]]
        name = books
        order = 5
        priority = -100
      '';
      };
  };
}
