{ config, lib, ... }:
let
  devices = config.custom.zigbee2mqtt.devices;
in
{
  # Home Assistant entity IDs are not reliably derivable from Zigbee2MQTT friendly names.
  # Before adding a device here, verify its actual entity IDs in:
  # Home Assistant → Settings → Devices & services → MQTT → device → MQTT info.
  # Use the IDs listed under "Entities". The MQTT discovery topic uses the IEEE address,
  # but Home Assistant may still create entity IDs from either the friendly name or IEEE.
  options.custom.services.homeAssistant.rooms = lib.mkOption {
    default = {};
    type    = lib.types.attrsOf (lib.types.submodule ({ name, config, lib, ... }: {
      options = {
        name = lib.mkOption { type = lib.types.str; };
        icon = lib.mkOption { type = lib.types.str; };

        # Derived from the attrset key: master_bedroom → master-bedroom
        path = lib.mkOption {
          type    = lib.types.str;
          default = lib.replaceStrings [ "_" ] [ "-" ] name;
        };

        plugs = lib.mkOption {
          default = {};
          type    = lib.types.attrsOf (lib.types.submodule ({ config, ... }: {
            options = {
              name           = lib.mkOption { type = lib.types.str; };
              device         = lib.mkOption { type = lib.types.str; description = "Friendly name of the zigbee device (from custom.zigbee2mqtt.devices.*.name)."; };
              # Plug entity IDs are mixed in HA: some use friendly names, others use IEEE.
              # Keep friendly-name defaults for existing plugs, override per plug when MQTT info shows IEEE IDs.
              switch         = lib.mkOption { type = lib.types.str; default = "switch.${config.device}"; };
              power          = lib.mkOption { type = lib.types.str; default = "sensor.${config.device}_power"; };
              energy         = lib.mkOption { type = lib.types.str; default = "sensor.${config.device}_energy"; };
              notifyWhenDone = lib.mkEnableOption "notify when appliance cycle finishes";
            };
          }));
        };

        contactSensors = lib.mkOption {
          default = {};
          type    = lib.types.attrsOf (lib.types.submodule ({ config, ... }: {
            options = {
              name    = lib.mkOption { type = lib.types.str; };
              device  = lib.mkOption { type = lib.types.str; description = "Friendly name of the zigbee device (from custom.zigbee2mqtt.devices.*.name)."; };
              ieee    = lib.mkOption { type = lib.types.str; description = "IEEE address of the zigbee device."; };
              # Third Reality door sensors expose contact/battery entity IDs from IEEE, not friendly name.
              contact = lib.mkOption { type = lib.types.str; default = "binary_sensor.${config.ieee}_contact"; };
              battery = lib.mkOption { type = lib.types.str; default = "sensor.${config.ieee}_battery"; };
            };
          }));
        };
      };
    }));
  };

  config = lib.mkIf config.services.home-assistant.enable {
    custom.services.homeAssistant.rooms = {
      office = {
        name  = "Office";
        icon  = "mdi:desk";
        plugs.desk = {
          name   = "Desk Plug";
          device = devices.office_plug.name;
        };
        contactSensors.door = {
          name   = "Office Door";
          device = devices.office_door.name;
          ieee   = devices.office_door.ieee;
        };
      };
      master_bedroom = {
        name = "Master Bedroom";
        icon = "mdi:bed";
      };
      master_bathroom = {
        name  = "Master Bathroom";
        icon  = "mdi:shower";
        contactSensors.door = {
          name   = "Master Bathroom Door";
          device = devices.master_bathroom_door.name;
          ieee   = devices.master_bathroom_door.ieee;
        };
        plugs.washing_machine = {
          name           = "Washing Machine";
          device         = devices.washing_machine.name;
          notifyWhenDone = true;
        };
      };
      bathroom = {
        name = "Bathroom";
        icon = "mdi:shower";
      };
      living_room = {
        name = "Living Room";
        icon = "mdi:sofa";
        plugs.main = {
          # MQTT info shows this plug uses IEEE-based entity IDs.
          name   = "Living Room Plug";
          device = devices.living_room.name;
          switch = "switch.${devices.living_room.ieee}";
          power  = "sensor.${devices.living_room.ieee}_power";
          energy = "sensor.${devices.living_room.ieee}_energy";
        };
      };
    };
  };
}
