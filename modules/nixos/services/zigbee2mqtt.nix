{ config, lib, ... }:
let
  zigbeeDevices = config.custom.zigbee2mqtt.devices;
in
{
  options.custom = {
    servicePort.zigbee2mqttFrontend = lib.mkOption {
      type = lib.types.int;
      default = 8124;
      description = "zigbee2mqtt port";
    };

    # used for to pre define zigbee devices based on their IEEE number.
    # this attribute set was created in order to eliminate variable duplications.
    #
    # 1. it is used to predefine zigbee devices(friendly_name based on ieee) for Zigbee2MQTT.
    #
    # 2. As home-assistant is using the friendly_names in its configuration, this configuration
    # is used by home-assistant to define them.
    zigbee2mqtt.devices = lib.mkOption {
      default = {};
      description = "Zigbee devices keyed by a short identifier, each with a friendly name and IEEE address. Consumed by zigbee2mqtt and other services such as Home Assistant.";
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          name = lib.mkOption { type = lib.types.str; description = "Friendly name used in zigbee2mqtt and derived entity IDs."; };
          ieee = lib.mkOption { type = lib.types.str; description = "IEEE 802.15.4 hardware address of the device."; };
        };
      });
    };
  };

  config = lib.mkIf config.services.zigbee2mqtt.enable {
    # pre defining zigbee devices to give them friendly names based on their ieee.
    custom.zigbee2mqtt.devices = {
      office_plug     = { name = "office_plug";                        ieee = "0xa4c1385bfbc8a447"; };
      office_door         = { name = "office_door";                        ieee = "0xb40e060fffe6e6cf"; };
      master_bathroom_door = { name = "master_bathroom_door";             ieee = "0xb40e060fffe6e6f4"; };
      washing_machine     = { name = "master_bedroom_washing_machine_plug"; ieee = "0xa4c138f6a0b26040"; };
      sink            = { name = "master_bedroom_sink_plug";            ieee = "0xa4c138f734afecb7"; };
      living_room     = { name = "living_room_plug";                    ieee = "0xa4c138b761af3b27"; };
      living_room_climate = { name = "living_room_climate";                 ieee = "0xa4c1380a1db9ffff"; };
    };

    # /var/lib/zigbee2mqtt/configuration.yaml
    # used to pair with zigbee devices.
    services.zigbee2mqtt = {
      settings = {
        # The serial port for the Zigbee Coordinator doungle.
        serial = {
          port = "/dev/serial/by-id/usb-ITead_Sonoff_Zigbee_3.0_USB_Dongle_Plus_8096431ecf3aef1195062c1455516304-if00-port0";
          # Why we use `/dev/serial/by-id/` instead of `/dev/ttyUSB0`:
          # Standard paths like `/dev/ttyUSB0` are assigned dynamically and can change across reboots or when plugging in other USB devices.
          # The `/dev/serial/by-id/` path is static and tied to the unique hardware string, ensuring Zigbee2MQTT always finds the dongle.
          #
          # Adapter type is set to "zstack" based on the hardware of the "P" variant:
          # 1. The USB-to-Serial bridge chip (via `lsusb`): The "P" model uses a Silicon Labs CP210x chip (ID 10c4:ea60),
          #    while the "E" model uses a WCH CH9102F chip. Our system shows the CP210x chip.
          # 2. The device name string: The `/dev/serial/by-id/` path lacks the "_V2" suffix, confirming it's the "P" model
          #    ("ITead_Sonoff_Zigbee_3.0_USB_Dongle_Plus").
          #
          # The "P" model uses a Texas Instruments CC2652P Zigbee chip running Z-Stack firmware.
          # Therefore, the required adapter setting in Zigbee2MQTT is "zstack" to communicate using the Z-Stack protocol language.
          adapter = "zstack";
        };
        #specifies the MQTT broker (Mosquitto) that Zigbee2MQTT will connect to for publishing and subscribing to MQTT topics.
        mqtt.server = "mqtt://${config.custom.shared.localHostIPv4}:${builtins.toString config.custom.servicePort.mosquitto}";  # Mosquitto MQTT broker
        frontend = {
          port = config.custom.servicePort.zigbee2mqttFrontend;
        };
        advanced.log_level = "info";

        # IMPORTANT: paring new devices.
        # when devices config are not set in any way. each time device is paried(by enabling "permit join in the gui), it will be addeed to the
        # /var/lib/zigbee2mqtt/devices.yaml file.
        #
        # By using this configuration two things will happen:
        #
        # 1. devices configuration will move into /var/lib/zigbee2mqtt/configuration.yaml will act at the sole source of throuth.
        #    making the devices.yaml file irrilavent.
        # 2. Eeah time we pair device it will be added to /var/lib/zigbee2mqtt/configuration.nix but unless it is defined as part of
        #    custom.zigbee2mqtt.devices it will be silintly removed next time the configuration are rebuilt.
        #
        # The floow with paring new devices with declerative configuration:
        # 1. Use gui to pair the new device like you always did.
        # 2. Once device is paried get its ieee value and add a new line into custom.zigbee2mqtt.devices with ieee and the friendly name.
        # 3. reuibld the configurations, if all gues well you should see you device in the gui with the firendly name you defined.
        devices = lib.mapAttrs' (_: device:
          lib.nameValuePair device.ieee {
            friendly_name = device.name;
          }
        ) zigbeeDevices;
      };
    };

    networking.firewall.allowedTCPPorts = [ config.custom.servicePort.zigbee2mqttFrontend ];
  };
}
