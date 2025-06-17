{ config, lib, ... }:
{
  options.customOptions.servicePort.zigbee2mqttFrontend = lib.mkOption {
    type = lib.types.int;
    default = 8124;
    description = "zigbee2mqtt port";
  };

  config = {
    # /var/lib/zigbee2mqtt/configuration.yaml
    # used to pair with zigbee devices.
    services.zigbee2mqtt = {
      enable = true;
      settings = {
        # The serial port for the Zigbee Coordinator doungle.
        serial.port = "/dev/serial/by-id/usb-ITead_Sonoff_Zigbee_3.0_USB_Dongle_Plus_8096431ecf3aef1195062c1455516304-if00-port0";
        #specifies the MQTT broker (Mosquitto) that Zigbee2MQTT will connect to for publishing and subscribing to MQTT topics.
        mqtt.server = "mqtt://${config.customGlobalOptions.localHostIPv4}:${builtins.toString config.customOptions.servicePort.mosquitto}";  # Mosquitto MQTT broker
        frontend = {
          port = config.customOptions.servicePort.zigbee2mqttFrontend;  # Change to your desired port
        };
        advanced.log_level = "info";  # Set log level
      };
    };

    networking.firewall.allowedTCPPorts = [ config.customOptions.servicePort.zigbee2mqttFrontend ];  # Open port for Web UI
  };
}
