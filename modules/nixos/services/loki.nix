# +------------+    Logs    +---------+    Chunks + Index    +----------+
# | Promtail   | ---------> | Loki    | ------------------> | /var/lib/loki |
# | (log shipper) |         | (ingester) |                  | (chunks + index) |
# +------------+             +---------+                    +----------+
#                                      |
#                                      v
#                                  Grafana queries Loki, Loki uses the index to find the right chunks.
{ config, ... }:
let
  loki_path_prefix = "/var/lib/loki";
  log_perioud = "24h";
in
{
services.loki = {
  enable = true;
  dataDir = loki_path_prefix;

  configuration = {
    common = {
      path_prefix = loki_path_prefix;
    };

    # defines:
    # defines how index is stored
    # what kind of storage is used
    # where the logs stored(local file system in this case).
    schema_config = {
      configs = [{
        from = "2024-01-01";
        store = "tsdb";
        object_store = "filesystem"; # both chunks and indexes are stored as plain files locally.
        schema = "v13";
        # maps labels to specific chunk. and set to be created every 24h.
        index = {
          prefix = "index_";
          period = "${log_perioud}";
        };
      }];
    };

    # Where to save chunks and indexes.
    storage_config = {
      tsdb_shipper = {
        active_index_directory = "${loki_path_prefix}/tsdb-shipper-active";
        cache_location = "${loki_path_prefix}/tsdb-shipper-cache";
      };
      filesystem = {
        directory = "${loki_path_prefix}/chunks";
      };
    };

    # Limits control how much data Loki will accept, store, and allow users to query.
    # These limits apply globally to all logs stored in Loki.
    #
    limits_config = {
      # Maximum retention period for logs. Any logs older than this will be deleted.
      retention_period = "${log_perioud}";

      # Maximum allowed size for a single log line. This prevents accidentally sending
      # very large or malformed logs that could break ingestion.
      max_line_size = 262144; # 256 KB

      # Maximum number of unique labels per log stream. This protects against excessive
      # cardinality (too many unique label combinations), which can hurt Loki performance.
      max_label_names_per_series = 30;

      # Maximum allowed length for label names and values.
      max_label_name_length = 1024;
      max_label_value_length = 2048;

      # Maximum range a query can cover in time. This limits very expensive queries.
      max_query_length = "${log_perioud}";

      # Maximum number of log chunks a single query can retrieve. This prevents runaway queries.
      max_chunks_per_query = 2000000;

      # Reject logs that are more than 1 week old at the time they are received.
      reject_old_samples_max_age = "1w";
    };


    # in memory cache + log processor.
    # Receiving incomifng logs -> builds chunks -> Periodically flushing those chunks to disk.
    #
    # chunk:
    # - It contains logs that have already been received, labeled, compressed, and encoded by Loki.
    # - Each chunk contains logs from a specific label set (like job=nginx, host=web01).
    # - Loki has already recorded in the index where this chunk lives, so it can find it later.
    # - After processing, the chunk is written to permanent storage (your dataDir in filesystem mode, or S3 in object store mode).
    ingester = {
      lifecycler = {
        address = "0.0.0.0";
        # keeps track of instances of loki, mostly irrelevant for a single node, but still needs to be defined.
        ring = {
          kvstore = {
            store = "inmemory"; # Single-node, so no need for etcd/consul.
          };
          replication_factor = 1; # Single node = no replication needed.
        };
      };
      # When promtail sends logs to loki, lokes troes these logs in "chunks".
      # a chink is a compressed file that holds the atual log lines.
      chunk_idle_period = "5m"; # If no logs come in, flush the chunk after 5m.
      chunk_retain_period = "30s"; # After flush, keep chunk in memory briefly for fast access.

      # Enable Write Ahead Log (WAL) for crash safety.
      wal = {
        enabled = true;
        dir = "${loki_path_prefix}/wal";  # Directory for WAL files.
      };
    };

    # the server grafana will be communicatoing with.
    server = {
      http_listen_port = 3100;
    };

    auth_enabled = false;
  };
};

}
