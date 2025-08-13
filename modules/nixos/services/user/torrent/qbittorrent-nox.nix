# Isn't configured for incoming connections.
{
  config,
  lib,
  pkgs,
  hostSpecific,
  ...
}:
let
  cfg = config.custom.services.qbittorrent;
in
{
  options.custom.services.qbittorrent = {
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
      group = "${config.customGlobal.mediaGroup}";
      home = cfg.dataDir;
      homeMode = "777";
      createHome = true;
      description = "qBittorrent service user";
    };

    users.groups.qbittorrent = { };
    users.users.${hostSpecific.primeUsername} = {
      extraGroups = [ "qbittorrent" ];
    };

    # Allows access to web-ui outside of localhost
    networking.firewall.allowedTCPPorts = [ cfg.port ];

    systemd.services.qbittorrent = {
      description = "qBittorrent Nox Service";
      after = [ config.systemd.targets.network.name];
      wantedBy = [ config.systemd.targets.multi-user.name];
      serviceConfig = {
        ExecStart = "${pkgs.qbittorrent-nox}/bin/qbittorrent-nox --webui-port=${toString cfg.port}";
        User = cfg.user;
        Restart = "always";
      };
    };
  };
}
