{ config, lib, ... }:
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
          type    = lib.types.attrsOf (lib.types.submodule {
            options = {
              name           = lib.mkOption { type = lib.types.str; };
              switch         = lib.mkOption { type = lib.types.str; };
              power          = lib.mkOption { type = lib.types.str; };
              energy         = lib.mkOption { type = lib.types.str; };
              notifyWhenDone = lib.mkEnableOption "notify when appliance cycle finishes";
            };
          });
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
          switch = "switch.office_plug";
          power  = "sensor.office_plug_power";
          energy = "sensor.office_plug_energy";
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
          switch         = "switch.master_bedroom_washing_machine_plug";
          power          = "sensor.master_bedroom_washing_machine_plug_power";
          energy         = "sensor.master_bedroom_washing_machine_plug_energy";
          notifyWhenDone = true;
        };
      };
      bathroom = {
        name = "Bathroom";
        icon = "mdi:shower";
      };
    };
  };
}
