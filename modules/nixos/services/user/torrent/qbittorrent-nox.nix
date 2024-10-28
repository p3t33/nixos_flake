{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.qbittorrent;
in
{
  options.services.qbittorrent = {
    enable = lib.mkEnableOption "qBittorrent headless service";

    dataDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/qbittorrent";
      description = "Directory where qBittorrent will store files.";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "qbittorrent";
      description = "User account under which qBittorrent runs.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8080;
      description = "qBittorrent web UI port.";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.qbittorrent = {
      isSystemUser = true;
      group = "media";
      home = cfg.dataDir;
      homeMode = "777";
      createHome = true;
      description = "qBittorrent service user";
    };

    users.groups.qbittorrent = { };
    users.users.${config.userDefinedGlobalVariables.primeUsername} = {
      extraGroups = [ "qbittorrent" ];
    };

    # Allows access to web-ui outside of localhost
    networking.firewall.allowedTCPPorts = [ cfg.port ];

    systemd.services.qbittorrent = {
      description = "qBittorrent Nox Service";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.qbittorrent-nox}/bin/qbittorrent-nox --webui-port=${toString cfg.port}";
        User = cfg.user;
        Restart = "always";
      };
    };
  };
}
