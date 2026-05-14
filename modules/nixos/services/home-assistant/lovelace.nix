{ config, lib, ... }:
let
  cfg = config.custom.services.homeAssistant;

  contactSensorCard = sensor: [
    {
      type     = "entities";
      title    = sensor.name;
      entities = [{
        entity = sensor.contact;
        name   = "State";
      }];
    }
    {
      type          = "sensor";
      name          = "Battery";
      entity        = sensor.battery;
      graph         = "line";
      hours_to_show = 168;
    }
  ];

  plugCard = plug: [
    {
      type     = "entities";
      title    = plug.name;
      entities = [ plug.switch ];
    }
    {
      type  = "vertical-stack";
      cards = [
        {
          type          = "sensor";
          name          = "Power";
          entity        = plug.power;
          graph         = "line";
          hours_to_show = 12;
        }
        {
          type       = "statistics-graph";
          title      = "Energy";
          entities   = [ plug.energy ];
          stat_types = [ "sum" ];
          period     = "day";
        }
      ];
    }
  ];

  environmentSensorCard = sensor: [
    {
      type     = "entities";
      title    = sensor.name;
      entities = [
        {
          entity = sensor.temperature;
          name   = "Temperature";
        }
        {
          entity = sensor.humidity;
          name   = "Humidity";
        }
      ];
    }
    {
      type          = "sensor";
      name          = "Battery";
      entity        = sensor.battery;
      graph         = "line";
      hours_to_show = 168;
    }
  ];

  navButton = room: {
    type      = "button";
    name      = room.name;
    icon      = room.icon;
    show_name = true;
    show_icon = true;
    tap_action = {
      action          = "navigate";
      navigation_path = "/lovelace/${room.path}";
    };
  };

  roomView = room: {
    title   = room.name;
    path    = room.path;
    icon    = room.icon;
    subview = true;
    cards   = lib.concatMap plugCard (lib.attrValues room.plugs)
            ++ lib.concatMap contactSensorCard (lib.attrValues room.contactSensors)
            ++ lib.concatMap environmentSensorCard (lib.attrValues room.environmentSensors);
  };

  infrastructureNavButton = {
    type      = "button";
    name      = "Infrastructure";
    icon      = "mdi:server-network";
    show_name = true;
    show_icon = true;
    tap_action = {
      action          = "navigate";
      navigation_path = "/lovelace/infrastructure";
    };
  };

  # Hardcoded — infrastructure devices are heterogeneous so no abstraction needed.
  # Add cards here as new infrastructure devices are integrated.
  infrastructureView = {
    title   = "Infrastructure";
    path    = "infrastructure";
    icon    = "mdi:server-network";
    subview = true;
    cards   = [
      {
        type  = "entities";
        title = "Zigbee2MQTT";
        entities = [
          "binary_sensor.zigbee2mqtt_bridge_connection_state"
          "sensor.zigbee2mqtt_bridge_version"
          "switch.zigbee2mqtt_bridge_permit_join"
        ];
      }
    ];
  };

in
{
  config = lib.mkIf config.services.home-assistant.enable {
    services.home-assistant.lovelaceConfig = {
      title = "Home";
      views =
        [{
          title = "Overview";
          path  = "overview";
          icon  = "mdi:home";
          cards =
            (lib.mapAttrsToList (_: navButton) cfg.rooms)
            ++ [ infrastructureNavButton ];
        }]
        ++ (lib.mapAttrsToList (_: roomView) cfg.rooms)
        ++ [ infrastructureView ];
    };
  };
}
