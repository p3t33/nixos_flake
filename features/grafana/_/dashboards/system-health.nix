# System Health dashboard — CPU, memory, disk, network from node_exporter.
{ lib, defaultTimeRange }:
let
  ds = { type = "prometheus"; uid = "prometheus"; };
  timeseries = title: expr: legendFormat: gridPos: {
    inherit title gridPos;
    type = "timeseries";
    datasource = ds;
    targets = [{ inherit expr legendFormat; refId = "A"; }];
    fieldConfig.defaults.custom = {
      drawStyle = "line";
      lineInterpolation = "smooth";
      fillOpacity = 10;
      showPoints = "never";
    };
  };
  timeseriesMulti = title: targets: gridPos: {
    inherit title gridPos targets;
    type = "timeseries";
    datasource = ds;
    fieldConfig.defaults.custom = {
      drawStyle = "line";
      lineInterpolation = "smooth";
      fillOpacity = 10;
      showPoints = "never";
    };
  };
  percentStat = title: expr: gridPos: {
    inherit title gridPos;
    type = "stat";
    datasource = ds;
    targets = [{ inherit expr; refId = "A"; }];
    fieldConfig.defaults = {
      unit = "percent";
      thresholds = {
        mode = "absolute";
        steps = [
          { color = "green"; value = null; }
          { color = "yellow"; value = 70; }
          { color = "red"; value = 90; }
        ];
      };
    };
    options.reduceOptions.calcs = [ "lastNotNull" ];
  };
in
{
  uid = "system-health";
  title = "System Health";
  tags = [ "system" "prometheus" "node-exporter" ];
  editable = true;
  time = { from = "now-${defaultTimeRange}"; to = "now"; };
  refresh = "30s";

  panels = [
    # Row 0: stat overview
    (percentStat
      "CPU Usage"
      "100 - (avg(rate(node_cpu_seconds_total{mode='idle'}[5m])) * 100)"
      { h = 4; w = 6; x = 0; y = 0; })
    (percentStat
      "Memory Usage"
      "100 * (1 - node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)"
      { h = 4; w = 6; x = 6; y = 0; })
    (percentStat
      "Disk Usage (/)"
      "100 - (node_filesystem_avail_bytes{mountpoint='/'} / node_filesystem_size_bytes{mountpoint='/'} * 100)"
      { h = 4; w = 6; x = 12; y = 0; })
    {
      title = "Uptime";
      type = "stat";
      datasource = ds;
      gridPos = { h = 4; w = 6; x = 18; y = 0; };
      targets = [{ expr = "node_time_seconds - node_boot_time_seconds"; refId = "A"; }];
      fieldConfig.defaults = {
        unit = "s";
        thresholds = {
          mode = "absolute";
          steps = [{ color = "green"; value = null; }];
        };
      };
      options.reduceOptions.calcs = [ "lastNotNull" ];
    }

    # Row 1: CPU + Memory timeseries
    (timeseries
      "CPU Usage Over Time"
      "100 - (avg by(instance) (rate(node_cpu_seconds_total{mode='idle'}[5m])) * 100)"
      "CPU %"
      { h = 8; w = 12; x = 0; y = 4; })
    (timeseries
      "Memory Usage Over Time"
      "100 * (1 - node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)"
      "Memory %"
      { h = 8; w = 12; x = 12; y = 4; })

    # Row 2: Disk + Network
    (timeseries
      "Disk Usage (/) Over Time"
      "100 - (node_filesystem_avail_bytes{mountpoint='/'} / node_filesystem_size_bytes{mountpoint='/'} * 100)"
      "Disk %"
      { h = 8; w = 12; x = 0; y = 12; })
    {
      title = "Network Traffic";
      type = "timeseries";
      datasource = ds;
      gridPos = { h = 8; w = 12; x = 12; y = 12; };
      targets = [
        { expr = "rate(node_network_receive_bytes_total{device!='lo'}[5m]) * 8"; legendFormat = "Download ({{device}})"; refId = "A"; }
        { expr = "rate(node_network_transmit_bytes_total{device!='lo'}[5m]) * 8"; legendFormat = "Upload ({{device}})"; refId = "B"; }
      ];
      fieldConfig.defaults = {
        unit = "bps";
        custom = {
          drawStyle = "line";
          lineInterpolation = "smooth";
          fillOpacity = 10;
          showPoints = "never";
        };
      };
    }

    # Row 3: Load average
    (timeseriesMulti
      "System Load"
      [
        { expr = "node_load1"; legendFormat = "1 min"; refId = "A"; }
        { expr = "node_load5"; legendFormat = "5 min"; refId = "B"; }
        { expr = "node_load15"; legendFormat = "15 min"; refId = "C"; }
      ]
      { h = 8; w = 24; x = 0; y = 20; })
  ];
}
