{config, lib, hostSpecific, ...}:
let
  pathToUsenetDirectory = "${config.custom.shared.pathToMediaDirectory}/usenet";
  categories = config.custom.media.downloadCategories;
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
      configFile = null;
      allowConfigWrite = true;
      secretFiles = [ config.sops.templates."sabnzbd-secrets.ini".path ];
      settings = {
        misc = {
          host = config.custom.shared.anyIPv4;
          port = config.custom.services.sabnzbd.httpPort;
          url_base = "/sabnzbd";
          host_whitelist = "nas, sabnzbd.nas";
          fixed_ports = true;
          download_dir = "${pathToUsenetDirectory}/incomplete";
          complete_dir = "${pathToUsenetDirectory}/complete";
          # admin_dir and log_dir must be absolute paths or SABnzbd resolves them
          # relative to the config file location and fails to write.
          admin_dir = "/var/lib/sabnzbd/admin";
          log_dir = "/var/lib/sabnzbd/logs";
          permissions = 775;
          cache_limit = "1G";
        };
        categories = {
          "*" = {
            name = "*";
            order = 0;
            pp = 3;
            script = "None";
            priority = 0;
          };
          "${categories.movies}" = {
            name = categories.movies;
            order = 1;
            priority = -100;
          };
          "${categories.tv}" = {
            name = categories.tv;
            order = 2;
            priority = -100;
          };
          "${categories.audio}" = {
            name = categories.audio;
            order = 3;
            priority = -100;
          };
          "${categories.software}" = {
            name = categories.software;
            order = 4;
            priority = -100;
          };
          "${categories.books}" = {
            name = categories.books;
            order = 5;
            priority = -100;
          };
        };
      };
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



    sops.templates."sabnzbd-secrets.ini" = {
      owner = config.services.sabnzbd.user;
      group = config.services.sabnzbd.group;
      mode = "0400";
      restartUnits = [ config.systemd.services.sabnzbd.name ];
      content = ''
        [misc]
        api_key = "${config.sops.placeholder."sabnzbd/api_key"}"
        nzb_key = "${config.sops.placeholder."sabnzbd/nzb_key"}"
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
      '';
    };
  };
}
