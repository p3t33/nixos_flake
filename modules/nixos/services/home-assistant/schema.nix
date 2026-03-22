{ config, lib, ... }:
let
  devices = config.custom.zigbee2mqtt.devices;
in
{
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
              switch         = lib.mkOption { type = lib.types.str; default = "switch.${config.device}"; };
              power          = lib.mkOption { type = lib.types.str; default = "sensor.${config.device}_power"; };
              energy         = lib.mkOption { type = lib.types.str; default = "sensor.${config.device}_energy"; };
              notifyWhenDone = lib.mkEnableOption "notify when appliance cycle finishes";
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
      };
      master_bedroom = {
        name = "Master Bedroom";
        icon = "mdi:bed";
      };
      master_bathroom = {
        name  = "Master Bathroom";
        icon  = "mdi:shower";
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
          name   = "Living Room Plug";
          device = devices.living_room.name;
        };
      };
    };
  };
}
