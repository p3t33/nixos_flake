{ config, pkgs, lib, ... }:

{
  sops.secrets."sonarr/apiKey" = {
    mode = "0644";
  };

  networking.firewall.allowedTCPPorts = [ config.userDefinedGlobalVariables.servicePort.prometheus.server ];

  services.prometheus = {
    enable = true;
    port = config.userDefinedGlobalVariables.servicePort.prometheus.server;
    retentionTime = "7d";
    extraFlags = [
      "--storage.tsdb.retention.size=5GB"
    ];

    globalConfig.scrape_interval = "10s"; # Scrape metrics every 10 seconds

  # Responsible to collect and provide metrics to the scrapers
    exporters = {
    # collects systemd metrics (CPU, RAM, etc.).
      node = {
        enable = true;
        extraFlags = [ "--collector.cpu" ];
        port = config.userDefinedGlobalVariables.servicePort.prometheus.nodeExporter;
      };

      # http, TCP, DNS, ICMP, gRPC (experimental), useful for service health status.
      blackbox = {
        enable = true;
        port = config.userDefinedGlobalVariables.servicePort.prometheus.blackboxExporter;
        configFile = pkgs.writeText "blackbox.yaml" ''
          modules:
            http_2xx:
              prober: http
              timeout: 5s
              http:
                valid_http_versions: ["HTTP/1.1", "HTTP/2"]
                method: GET
            icmp_probe:
              prober: icmp
              timeout: 5s
              icmp:
                preferred_ip_protocol: ip4
            tcp_connect:
              prober: tcp
              timeout: 5s
              tcp:
                preferred_ip_protocol: ip4
        '';
      };

      # sonarr metrics
      exportarr-sonarr = lib.mkIf config.services.sonarr.enable {
        enable = true;
        url = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.sonarr}/sonarr";
        port = config.userDefinedGlobalVariables.servicePort.prometheus.sonarrExporter;
        apiKeyFile = config.sops.secrets."sonarr/apiKey".path;
        openFirewall = true;
      };

    };

    # responsible to scrape the data from the exporters.
    scrapeConfigs = [ ]
      # sonarr health check.
      ++ lib.optionals (config.services.sonarr.enable && config.services.prometheus.exporters.blackbox.enable) [
        {
          job_name = "blackbox_sonarr";
          metrics_path = "/probe";
          params.module = [ "http_2xx" ];
          static_configs = [{
              targets = [ "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.sonarr}/sonarr" ];
          }];
          relabel_configs = [
          { source_labels = [ "__address__" ]; target_label = "__param_target"; }
          { source_labels = [ "__param_target" ]; target_label = "instance"; }
          { target_label = "__address__"; replacement = "${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.prometheus.blackboxExporter}"; }
          ];
        }
      ]
      # sonarr metrics.
      ++ lib.optionals config.services.prometheus.exporters.exportarr-sonarr.enable [
        {
          job_name = "sonarr";
          static_configs = [{ targets = [ "${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.prometheus.sonarrExporter}" ]; }];
        }
      ]
      ++ lib.optionals config.services.prometheus.exporters.node.enable [
        {
          job_name = "node_exporter";
          static_configs = [{ targets = [ "${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.prometheus.nodeExporter}" ]; }];
        }
      ]
      ++ lib.optionals config.services.restic.server.enable [
        {
          job_name = "restic";
          metrics_path = "/metrics";
          static_configs = [{ targets = [ "${config.userDefinedGlobalVariables.localHostIPv4}:9005" ]; }];
        }
      ];
  };
}
