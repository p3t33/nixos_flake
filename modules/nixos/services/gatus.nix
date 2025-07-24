{ config, lib, ... }:
let
  media = "media";
  monitoring = "monitoring";
  files = "files";
  filtering = "filtering";
  remoteAccess =" remote access";
  external = "external";
in
{
  config = lib.mkIf config.services.gatus.enable {
    sops.secrets.gatus = {
      restartUnits = [ config.systemd.services.gatus.name ];
    };

    services.gatus = {
      openFirewall = true; # Allows access to the Gatus UI from your network
      environmentFile = config.sops.secrets.gatus.path;
      settings = {
        ui = {
          title = "homelab Status";
          dark-mode = true;
        };

        storage = {
          type = "postgres";
          # passwrod for gatus postgresql database is stored inside of environmentFile provided to the gatus service.
          path = "postgres://${config.systemd.services.gatus.serviceConfig.User}:\${GATUS_DB_PASSWORD}@${config.customGlobal.localHostIPv4}:${builtins.toString config.services.postgresql.settings.port}/${config.systemd.services.gatus.serviceConfig.User}?sslmode=disable";
          caching = true;
          maximum-number-of-results = 800;
          maximum-number-of-events = 50;
      };

        alerting = {
          telegram = {
            # For this to work the config.sops.secrets.gatus.path needs to have those variables defined:
            #
            # TELEGRAM_TOKEN=<your bot token>
            # TELEGRAM_CHAT_ID=<your bot chat id>
            token = "\${TELEGRAM_TOKEN}";
            id = "\${TELEGRAM_CHAT_ID}";
          };
        };

        web = {
          address = "${config.customGlobal.anyIPv4}"; # Listen on all interfaces
          port = 8081;
          root = "/gatus";
        };

        endpoints =
        [
          {
            name = "blog";
            group = external;
            url = "https://www.kobimedrish.com";
            interval = "1m";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "blog is down!";
            }];
      }

        ]
        ++ lib.optionals config.services.openssh.enable [
          {
            name = "sshd";
            group = "${remoteAccess}";
            url = "tcp://${config.customGlobal.localHostIPv4}:22";
            interval = "30s";
            conditions = [
              "[CONNECTED] == true"
            ];

            alerts = [
            {
              type = "telegram";
              enabled = true;
              failure-threshold = 3;
              success-threshold = 1;
              description = "SSH service is unreachable";
            }
            ];
          }
        ]
        ++ lib.optionals config.services.calibre-web.enable [
          {
            name = "calibre-web";
            group = "${media}";
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.services.calibre-web.listen.port}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [
            {
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "Calibre-Web is down!";
            }
            ];
          }
        ]
        ++ lib.optionals config.services.deluge.enable [
          {
            name = "deluge";
            group = media;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.services.deluge.web.port}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "Deluge is down!";
            }];
          }
        ]
        ++ lib.optionals config.services.sabnzbd.enable [
          {
            name = "sabnzbd";
            group = media;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.custom.servicePort.sabnzbd}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "SABnzbd is down!";
            }];
          }
        ]
        ++ lib.optionals config.services.bazarr.enable [
          {
            name = "bazarr";
            group = media;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.services.bazarr.listenPort}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "Bazarr is down!";
            }];
          }
        ]
        ++ lib.optionals config.services.sonarr.enable [
          {
            name = "sonarr";
            group = media;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.services.sonarr.settings.server.port}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "Sonarr is down!";
            }];
          }
        ]
        ++ lib.optionals config.services.readarr.enable [
          {
            name = "readarr";
            group = media;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.services.readarr.settings.server.port}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "Readarr is down!";
            }];
          }
        ]
        ++ lib.optionals config.services.radarr.enable [
          {
            name = "radarr";
            group = media;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.services.radarr.settings.server.port}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "Radarr is down!";
            }];
          }
        ]
        ++ lib.optionals config.services.prowlarr.enable [
          {
            name = "prowlarr";
            group = media;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.services.prowlarr.settings.server.port}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "Prowlarr is down!";
            }];
          }
        ]
        ++ lib.optionals config.services.jackett.enable [
          {
            name = "jackett";
            group = media;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.services.jackett.port}/favicon.ico";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "Jackett is down!";
            }];
          }
        ]
        ++ lib.optionals config.services.jellyfin.enable [
          {
            name = "jellyfin";
            group = media;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.custom.servicePort.jellyfin}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "Jellyfin is down!";
            }];
          }
        ]
        ++ lib.optionals config.services.prometheus.enable [
          {
            name = "prometheus";
            group = monitoring;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.services.prometheus.port}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "Prometheus is down!";
            }];
          }
        ]
        ++ lib.optionals config.services.grafana.enable [
          {
            name = "grafana";
            group = monitoring;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.services.grafana.settings.server.http_port}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "Grafana is down!";
            }];
          }
        ]
        ++ lib.optionals config.services.syncthing.enable [
          {
            name = "syncthing";
            group = files;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.custom.services.syncthing.httpPort}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "Syncthing is down!";
            }];
          }
        ]
        ++ lib.optionals config.services.adguardhome.enable [
          {
            name = "adguard";
            group = filtering;
            url = "http://${config.customGlobal.localHostIPv4}:${toString config.services.adguardhome.port}";
            interval = "30s";
            conditions = [ "[STATUS] == 200" ];
            alerts = [{
              type = "telegram";
              enabled = true;
              failure-threshold = 2;
              success-threshold = 1;
              description = "AdGuard Home is down!";
            }];
          }
        ];
      };
    };

    # gatus will fail to load if the database isn"t ready yet."
    systemd.services.gatus = {
      after = [ config.systemd.services.postgresql.name];
      requires = [ config.systemd.services.postgresql.name];
    };

  };
}
