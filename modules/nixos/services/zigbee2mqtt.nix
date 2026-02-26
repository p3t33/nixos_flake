{ config, lib, ... }:
{
  options.custom = {
    servicePort.zigbee2mqttFrontend = lib.mkOption {
      type = lib.types.int;
      default = 8124;
      description = "zigbee2mqtt port";
    };
  };

  config = lib.mkIf config.services.zigbee2mqtt.enable {
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
          port = config.custom.servicePort.zigbee2mqttFrontend;  # Change to your desired port
        };
        advanced.log_level = "info";  # Set log level
      };
    };

    networking.firewall.allowedTCPPorts = [ config.custom.servicePort.zigbee2mqttFrontend ];  # Open port for Web UI
  };
}
