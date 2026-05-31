# Replaces promtail (EOL) as the log shipper for the monitoring stack.
# Reads systemd journal, relabels unit and priority, pushes to Loki.
{ config, lib, ... }:
{
  config = lib.mkIf config.services.alloy.enable {
    environment.etc."alloy/config.alloy" = {
      text = ''
        loki.relabel "journal" {
          forward_to = []

          rule {
            source_labels = ["__journal__systemd_unit"]
            target_label  = "unit"
          }

          rule {
            source_labels = ["__journal_priority_keyword"]
            target_label  = "level"
          }
        }

        loki.source.journal "read" {
          forward_to    = [loki.write.endpoint.receiver]
          relabel_rules = loki.relabel.journal.rules
          labels        = {job = "systemd-journal"}
          max_age       = "12h"
        }

        loki.write "endpoint" {
          endpoint {
            url = "http://${config.custom.shared.localHostIPv4}:${builtins.toString config.services.loki.configuration.server.http_listen_port}/loki/api/v1/push"
          }
        }
      '';
    };
  };
}
