{config, pkgs, lib, ...}:
{
  networking.firewall.allowedTCPPorts = [ config.userDefinedGlobalVariables.servicePort.grafana ];

  services.grafana = {
    enable = true;
    settings = {
        users.home_page = "/d/home-dashboard";
        server = {
          http_addr = "0.0.0.0";
          http_port = config.userDefinedGlobalVariables.servicePort.grafana;
       };
    };

    # Some dashbaords have plugins dependencies.
    # TODO: make them confitional in the future.
    declarativePlugins = with pkgs.grafanaPlugins; [
      grafana-piechart-panel
      grafana-clock-panel
    ];

    provision = {
      enable = true;

      # The sources of data for grafana to query.
      datasources.settings.datasources =
        [ ]
        ++ lib.optionals config.services.prometheus.enable [
          {
            name = "Prometheus";
            type = "prometheus";
            url = "http://${config.services.prometheus.listenAddress}:${toString config.services.prometheus.port}";
            access = "proxy";
            isDefault = true;
          }
        ]
        ++ lib.optionals config.services.loki.enable [
          {
            name = "Loki";
            type = "loki";
            url = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.loki}";
            access = "proxy";
            jsonData = {
              maxLines = 1000;
            };
          }
        ];
      # automatically load dashboards from a filesystem path. It allows you to "provision"
      # dashboards instead of manually creating them in the UI, which is great in nix context.
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

  environment.etc."grafana-dashboards/loki.json" = {
  text = builtins.toJSON {
    uid = "loki-logs-dashboard";
    title = "System Logs (Loki)";
    tags = [ "logs" "loki" "systemd" ];
    time = {
      from = "now-1h";
      to = "now";
    };
    panels = [
      # 🔥 Logs Viewer (Main Log Stream)
      {
        title = "Systemd Logs (Last 1h)";
        type = "logs";
        datasource = "Loki";
        gridPos = { h = 16; w = 24; x = 0; y = 0; };
        targets = [
          {
            expr = "{job=\"systemd-journal\"}";
            refId = "A";
          }
        ];
        options = {
          showLabels = true;
          wrapLogMessage = true;
          sortOrder = "Descending";
        };
      }

      # 📊 Log Levels - String Matching (no JSON parsing)
      {
        title = "Info Logs (Last 1h)";
        type = "stat";
        datasource = "Loki";
        gridPos = { h = 4; w = 8; x = 0; y = 16; };
        targets = [
          {
            expr = "count_over_time({job=\"systemd-journal\"} |= \"INFO\" [1h])";
            refId = "B1";
          }
        ];
        options = {
          reduceOptions = {
            calcs = [ "last" ];
            fields = "";
            values = false;
          };
          orientation = "horizontal";
          textMode = "value";
        };
      }

      {
        title = "Warning Logs (Last 1h)";
        type = "stat";
        datasource = "Loki";
        gridPos = { h = 4; w = 8; x = 8; y = 16; };
        targets = [
          {
            expr = "count_over_time({job=\"systemd-journal\"} |= \"WARN\" [1h])";
            refId = "B2";
          }
        ];
        options = {
          reduceOptions = {
            calcs = [ "last" ];
            fields = "";
            values = false;
          };
          orientation = "horizontal";
          textMode = "value";
        };
      }

      {
        title = "Error Logs (Last 1h)";
        type = "stat";
        datasource = "Loki";
        gridPos = { h = 4; w = 8; x = 16; y = 16; };
        targets = [
          {
            expr = "count_over_time({job=\"systemd-journal\"} |= \"ERROR\" [1h])";
            refId = "B3";
          }
        ];
        options = {
          reduceOptions = {
            calcs = [ "last" ];
            fields = "";
            values = false;
          };
          orientation = "horizontal";
          textMode = "value";
        };
      }

      # 📊 Top Units - Systemd Unit Labels
      {
        title = "Top Logging Systemd Unit (Last 1h)";
        type = "stat";
        datasource = "Loki";
        gridPos = { h = 4; w = 24; x = 0; y = 20; };
        targets = [
          {
            expr = "topk(1, count_over_time({job=\"systemd-journal\"} [1h]) by (unit))";
            refId = "C";
          }
        ];
        options = {
          reduceOptions = {
            calcs = [ "last" ];
            fields = "";
            values = false;
          };
          orientation = "horizontal";
          textMode = "value_and_name";
        };
      }
    ];
  };
  user = "grafana";
  group = "grafana";
};

  environment.etc."grafana-dashboards/home.json" = {
  text = builtins.toJSON {
    uid = "home-dashboard";  # Unique identifier for the dashboard
    title = "Home Dashboard";
    tags = [ "home" ];
    panels = [
      {
        title = "Welcome";
        type = "text";
        mode = "markdown";
        content = "### Welcome to Grafana";
        gridPos = {
          h = 4;  # Adjusted height
          w = 24; # Full width
          x = 0;
          y = 0;
        };
      }
      {
        title = "CPU Usage";
        type = "graph";
        datasource = "Prometheus";
        targets = [
          {
            expr = "100 - (avg by(instance) (rate(node_cpu_seconds_total{mode='idle'}[5m])) * 100)";
            legendFormat = "CPU ussage";
          }
        ];
        gridPos = {
          h = 8;  # Increased height
          w = 12; # Half width
          x = 0;
          y = 4;
        };
      }
      {
        title = "Memory Usage";
        type = "graph";
        datasource = "Prometheus";
        targets = [
          {
            expr = "100 * (1 - (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes))";
            legendFormat = "Memory Used (%)";
          }
        ];
        gridPos = {
          h = 8;  # Increased height
          w = 12; # Half width
          x = 12;
          y = 4;
        };
      }
    ];
  };
  user = "grafana";
  group = "grafana";
};

  environment.etc."grafana-dashboards/node-exporter.json" = {
    text = builtins.toJSON {
      __inputs = [];
      __requires = [];
      title = "CPU & Memory Dashboard";
      description = "A basic dashboard for monitoring CPU and Memory usage";
      tags = [ "system" "cpu" "memory" "disk" "network" ];

    panels = [
      # 🔹 CPU Usage Panel
      {
        title = "CPU Usage";
        type = "graph";
        datasource = "Prometheus";
        targets = [
          {
            expr = "100 - (avg by(instance) (rate(node_cpu_seconds_total{mode='idle'}[5m])) * 100)";
            legendFormat = "{{instance}}";
          }
        ];
        gridPos = { h = 8; w = 12; x = 0; y = 0; };
      }

      # 🔹 Memory Usage Panel
      {
        title = "Memory Usage";
        type = "graph";
        datasource = "Prometheus";
        targets = [
          {
            expr = "100 * (1 - (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes))";
            legendFormat = "Memory Used";
          }
        ];
        gridPos = { h = 8; w = 12; x = 12; y = 0; };
      }

      # 🔹 Disk Usage Panel
      {
        title = "Disk Usage (%)";
        type = "graph";
        datasource = "Prometheus";
        targets = [
          {
            expr = "(node_filesystem_avail_bytes{mountpoint='/'} / node_filesystem_size_bytes{mountpoint='/'}) * 100";
            legendFormat = "Disk Free (%)";
          }
        ];
        gridPos = { h = 8; w = 12; x = 0; y = 8; };
      }

      # 🔹 Network Traffic Panel
      {
        title = "Network Traffic (Mbps)";
        type = "graph";
        datasource = "Prometheus";
        targets = [
          {
            expr = "rate(node_network_receive_bytes_total[5m]) / 1024 / 1024 * 8";
            legendFormat = "Download (Mbps)";
          }
          {
            expr = "rate(node_network_transmit_bytes_total[5m]) / 1024 / 1024 * 8";
            legendFormat = "Upload (Mbps)";
          }
        ];
        gridPos = { h = 8; w = 12; x = 12; y = 8; };
      }

      # 🔹 System Load Average Panel
      {
        title = "System Load Average";
        type = "graph";
        datasource = "Prometheus";
        targets = [
          {
            expr = "node_load1";
            legendFormat = "Load 1 min";
          }
          {
            expr = "node_load5";
            legendFormat = "Load 5 min";
          }
          {
            expr = "node_load15";
            legendFormat = "Load 15 min";
          }
        ];
        gridPos = { h = 8; w = 24; x = 0; y = 16; };
      }
    ];
    };
    user = "grafana";
    group = "grafana";
  };


  environment.etc."grafana-dashboards/restic.json" = {
    text = builtins.toJSON {
      title = "Restic REST Server Dashboard";
      description = "Monitoring Restic REST Server performance";
      tags = [ "restic" "backup" "system" ];
      panels = [
        {
          title = "Restic Server Memory Usage";
          type = "graph";
          datasource = "Prometheus";
          targets = [
            {
              expr = "process_resident_memory_bytes{job='restic'} / 1024 / 1024";
              legendFormat = "Memory Used (MB)";
            }
          ];
        }
        {
          title = "Restic Garbage Collection Time";
          type = "graph";
          datasource = "Prometheus";
          targets = [
            {
              expr = "rate(go_gc_duration_seconds_sum{job='restic'}[5m])";
              legendFormat = "GC Duration (sec)";
            }
          ];
        }
      ];
    };
    user = "grafana";
    group = "grafana";
  };
  environment.etc."grafana-dashboards/sonarr.json" = {
  text = builtins.toJSON {
    title = "Sonarr Dashboard";
    description = "Monitoring Sonarr's status and downloads";
    tags = [ "sonarr" "media" "system" ];
    panels = [
      # ✅ Sonarr Uptime Panel (Blackbox Exporter)
      {
        title = "Sonarr Uptime";
        type = "graph";  # Change from 'stat' to 'graph' to see the time series
        datasource = "Prometheus";
        targets = [
          {
            expr = "probe_success{job='blackbox_sonarr'}";
            legendFormat = "Sonarr Status";
          }
        ];
        gridPos = { h = 4; w = 8; x = 0; y = 0; };
        yaxes = [
          { format = "short"; min = 0; max = 1; }
        ];
        tooltip = { shared = true; };
      }

      # ✅ Sonarr Queue Size (From Exportarr-Sonarr)
      {
        title = "Sonarr Queue Size";
        type = "graph";
        datasource = "Prometheus";
        targets = [
          {
            expr = "sonarr_queue_total";
            legendFormat = "Total in Queue";
          }
        ];
        gridPos = { h = 6; w = 12; x = 0; y = 4; };
      }

      # ✅ Missing Episodes (From Exportarr-Sonarr)
      {
        title = "Missing Episodes";
        type = "graph";
        datasource = "Prometheus";
        targets = [
          {
            expr = "sonarr_episode_missing_total";
            legendFormat = "Missing Episodes";
          }
        ];
        gridPos = { h = 6; w = 12; x = 12; y = 4; };
      }

      # ✅ Total Episodes (From Exportarr-Sonarr)
      {
        title = "Total Episodes";
        type = "graph";
        datasource = "Prometheus";
        targets = [
          {
            expr = "sonarr_episode_total";
            legendFormat = "Total Episodes";
          }
        ];
        gridPos = { h = 6; w = 12; x = 0; y = 10; };
      }

      # ✅ Disk Space Usage for Sonarr (From Exportarr-Sonarr)
      {
        title = "Disk Space Used by Sonarr";
        type = "graph";
        datasource = "Prometheus";
        targets = [
          {
            expr = "sonarr_rootfolder_freespace_bytes / 1024 / 1024 / 1024";
            legendFormat = "Free Disk Space (GB)";
          }
        ];
        gridPos = { h = 6; w = 12; x = 12; y = 10; };
      }
    ];
  };
  user = "grafana";
  group = "grafana";
};

}
