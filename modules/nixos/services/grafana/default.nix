{ config, pkgs, lib, ... }:
let
  lokiRetention = config.services.loki.configuration.limits_config.retention_period;
  prometheusRetention = config.services.prometheus.retentionTime;

  dashboards = {
    system-health = import ./dashboards/system-health.nix { inherit lib; defaultTimeRange = prometheusRetention; };
    system-logs = import ./dashboards/system-logs.nix { inherit lib; defaultTimeRange = lokiRetention; };
  };
in
{
  config = lib.mkIf config.services.grafana.enable {

    sops.secrets."grafana/admin_password" = {
      owner = "grafana";
    };

    sops.secrets."grafana/secret_key" = {
      owner = "grafana";
    };

    networking.firewall.allowedTCPPorts = [ config.services.grafana.settings.server.http_port ];

    services.grafana = {
      settings = {
        users.home_page = "/d/system-health";
        server = {
          http_addr = "${config.custom.shared.anyIPv4}";
          http_port = 3001;
        };
        security = {
          admin_user = "admin";
          admin_password = "$__file{${config.sops.secrets."grafana/admin_password".path}}";
          # generated with
          # openssl rand -base64 32
          secret_key = "$__file{${config.sops.secrets."grafana/secret_key".path}}";
        };
      };

      declarativePlugins = with pkgs.grafanaPlugins; [
        grafana-clock-panel
        grafana-lokiexplore-app
        grafana-pyroscope-app
        grafana-exploretraces-app
        grafana-metricsdrilldown-app
      ];

      provision = {
        enable = true;

        datasources.settings.datasources =
          [ ]
          ++ lib.optionals config.services.prometheus.enable [
            {
              name = "Prometheus";
              uid = "prometheus";
              type = "prometheus";
              url = "http://${config.custom.shared.localHostIPv4}:${toString config.services.prometheus.port}";
              access = "proxy";
              isDefault = true;
            }
          ]
          ++ lib.optionals config.services.loki.enable [
            {
              name = "Loki";
              uid = "loki";
              type = "loki";
              url = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.loki.configuration.server.http_listen_port}";
              access = "proxy";
              jsonData.maxLines = 1000;
            }
          ];

        dashboards.settings.providers = lib.optionals (config.services.prometheus.enable || config.services.loki.enable) [
          {
            name = "Provisioned Dashboards";
            options.path = "/etc/grafana-dashboards";
            updateIntervalSeconds = 10;
            allowUiUpdates = true;
          }
        ];
      };
    };

    environment.etc = lib.mapAttrs' (name: dashboard:
      lib.nameValuePair "grafana-dashboards/${name}.json" {
        text = builtins.toJSON dashboard;
        user = "grafana";
        group = "grafana";
      }
    ) dashboards;
  };
}
