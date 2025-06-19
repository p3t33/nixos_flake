{ config, lib, ... }:
{
  config = lib.mkIf config.services.promtail.enable {
    services.promtail = {
      configuration = {
        server = {
          http_listen_port = 9080;
          grpc_listen_port = 0; # disables gRPC listener, no clustering
        };
        # tells promtail where to send logs
        clients = [ ]
          ++ lib.optionals config.services.loki.enable [
            {
              url = "http://${config.customGlobal.localHostIPv4}:${builtins.toString config.services.loki.configuration.server.http_listen_port}/loki/api/v1/push";
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
  };
}
