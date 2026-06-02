# System Logs dashboard — errors, noisy services, severity filtering from Loki.
{ lib, defaultTimeRange }:
let
  ds = { type = "loki"; uid = "loki"; };

  # Known-noisy log lines to exclude from all severity queries.
  # dbus-broker duplicate service file warnings (NixOS upstream wart).
  noiseFilter = ''!~ "Ignoring duplicate name"'';

  stat = title: expr: gridPos: extra: {
    inherit title gridPos;
    type = "stat";
    datasource = ds;
    targets = [{ inherit expr; refId = "A"; instant = true; }];
    options.reduceOptions.calcs = [ "lastNotNull" ];
  } // extra;
in
{
  uid = "system-logs";
  title = "System Logs";
  tags = [ "logs" "loki" "systemd" ];
  editable = true;
  time = { from = "now-${defaultTimeRange}"; to = "now"; };
  refresh = "30s";

  templating.list = [
    {
      type = "query";
      name = "level";
      label = "Level";
      datasource = ds;
      definition = "label_values(level)";
      query = { label = "level"; type = 1; };
      refresh = 2;
      multi = true;
      includeAll = true;
      allValue = ".*";
      sort = 1;
      current = {
        text = [ "crit" "error" "warning" ];
        value = [ "crit" "error" "warning" ];
      };
    }
    {
      type = "query";
      name = "unit";
      label = "Unit";
      datasource = ds;
      definition = "label_values(unit)";
      query = { label = "unit"; type = 1; };
      refresh = 2;
      multi = true;
      includeAll = true;
      allValue = ".*";
      sort = 1;
    }
  ];

  panels = [
    # Row 0: severity stats
    (stat
      "Critical"
      "sum(count_over_time({level=~\"emerg|alert|crit\"} ${noiseFilter} [$__range])) or vector(0)"
      { h = 4; w = 6; x = 0; y = 0; }
      { fieldConfig.defaults.thresholds = {
          mode = "absolute";
          steps = [
            { color = "green"; value = null; }
            { color = "purple"; value = 1; }
          ];
        };
      })
    (stat
      "Errors"
      "sum(count_over_time({level=\"error\"} ${noiseFilter} [$__range])) or vector(0)"
      { h = 4; w = 6; x = 6; y = 0; }
      { fieldConfig.defaults.thresholds = {
          mode = "absolute";
          steps = [
            { color = "green"; value = null; }
            { color = "red"; value = 1; }
          ];
        };
      })
    (stat
      "Warnings"
      "sum(count_over_time({level=\"warning\"} ${noiseFilter} [$__range])) or vector(0)"
      { h = 4; w = 6; x = 12; y = 0; }
      { fieldConfig.defaults.thresholds = {
          mode = "absolute";
          steps = [
            { color = "green"; value = null; }
            { color = "yellow"; value = 1; }
          ];
        };
      })
    (stat
      "Total Log Lines"
      "sum(count_over_time({job=\"systemd-journal\"} [$__range]))"
      { h = 4; w = 6; x = 18; y = 0; }
      { fieldConfig.defaults.thresholds = {
          mode = "absolute";
          steps = [
            { color = "blue"; value = null; }
          ];
        };
      })

    # Row 1: top units
    {
      title = "Top Logging Units";
      type = "table";
      datasource = ds;
      gridPos = { h = 8; w = 12; x = 0; y = 4; };
      targets = [{
        expr = "topk(10, sum by(unit) (count_over_time({job=\"systemd-journal\"} [$__range])))";
        refId = "A";
        instant = true;
      }];
      options.sortBy = [{ displayName = "Value #A"; desc = true; }];
      fieldConfig.defaults.color.fixedColor = "blue";
      fieldConfig.defaults.color.mode = "fixed";
      fieldConfig.overrides = [
        {
          matcher = { id = "byName"; options = "Time"; };
          properties = [{ id = "custom.hidden"; value = true; }];
        }
        {
          matcher = { id = "byName"; options = "Value #A"; };
          properties = [{
            id = "custom.cellOptions";
            value = { type = "gauge"; mode = "basic"; };
          }];
        }
      ];
    }
    {
      title = "Top Error-Producing Units";
      type = "table";
      datasource = ds;
      gridPos = { h = 8; w = 12; x = 12; y = 4; };
      targets = [{
        expr = "topk(10, sum by(unit) (count_over_time({job=\"systemd-journal\", level=~\"emerg|alert|crit|error|warning\", unit!=\"\"} ${noiseFilter} [$__range])))";
        refId = "A";
        instant = true;
      }];
      options.sortBy = [{ displayName = "Value #A"; desc = true; }];
      fieldConfig.defaults.color.fixedColor = "red";
      fieldConfig.defaults.color.mode = "fixed";
      fieldConfig.overrides = [
        {
          matcher = { id = "byName"; options = "Time"; };
          properties = [{ id = "custom.hidden"; value = true; }];
        }
        {
          matcher = { id = "byName"; options = "Value #A"; };
          properties = [{
            id = "custom.cellOptions";
            value = { type = "gauge"; mode = "basic"; };
          }];
        }
      ];
    }

    # Row 2: problem rate over time
    {
      title = "Problem Rate Over Time";
      type = "timeseries";
      datasource = ds;
      gridPos = { h = 6; w = 24; x = 0; y = 12; };
      targets = [
        {
          expr = "sum(count_over_time({level=~\"emerg|alert|crit\"} ${noiseFilter} [5m]))";
          legendFormat = "critical";
          refId = "A";
        }
        {
          expr = "sum(count_over_time({level=\"error\"} ${noiseFilter} [5m]))";
          legendFormat = "error";
          refId = "B";
        }
        {
          expr = "sum(count_over_time({level=\"warning\"} ${noiseFilter} [5m]))";
          legendFormat = "warning";
          refId = "C";
        }
      ];
      fieldConfig.defaults.custom = {
        drawStyle = "bars";
        fillOpacity = 100;
        stacking = { mode = "normal"; group = "A"; };
      };
      fieldConfig.overrides = [
        {
          matcher = { id = "byName"; options = "critical"; };
          properties = [{ id = "color"; value = { fixedColor = "purple"; mode = "fixed"; }; }];
        }
        {
          matcher = { id = "byName"; options = "error"; };
          properties = [{ id = "color"; value = { fixedColor = "red"; mode = "fixed"; }; }];
        }
        {
          matcher = { id = "byName"; options = "warning"; };
          properties = [{ id = "color"; value = { fixedColor = "yellow"; mode = "fixed"; }; }];
        }
      ];
    }

    # Row 3: log stream (main panel) — filtered by template variables
    {
      title = "Logs";
      type = "logs";
      datasource = ds;
      gridPos = { h = 16; w = 24; x = 0; y = 18; };
      targets = [{
        expr = "{job=\"systemd-journal\", level=~\"$level\", unit=~\"$unit\"} ${noiseFilter}";
        refId = "A";
      }];
      options = {
        showLabels = true;
        showTime = true;
        wrapLogMessage = true;
        sortOrder = "Descending";
        enableLogDetails = true;
      };
    }
  ];
}
