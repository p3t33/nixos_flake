{config, lib, ...}:
{
services.promtail = {
  enable = true;
  configuration = {
    server = {
      http_listen_port = config.userDefinedGlobalVariables.servicePort.promtail;
      grpc_listen_port = 0; # disables gRPC listener, no clustering
    };
    # tells promtail where to send logs
    clients = [ ]
      ++ lib.optionals config.services.loki.enable [
        {
          url = "http://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.loki}/loki/api/v1/push";
        }
    ];

    # defines what information is being scraped.
    scrape_configs = [ ]
      ++ lib.optionals config.services.loki.enable [{
        job_name = "journal";
        journal = {
          max_age = "12h";
          labels = {
            job = "systemd-journal";
          };
        };
        relabel_configs = [{
          source_labels = ["__journal__systemd_unit"];
          target_label = "unit";
        }];
      }];
    };
  };
}
