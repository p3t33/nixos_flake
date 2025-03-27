{ config, ... }:
{
  # /var/lib/zigbee2mqtt/configuration.yaml
  # used to pair with zigbee devices.
  services.zigbee2mqtt = {
    enable = true;
    settings = {
      # The serial port for the Zigbee Coordinator doungle.
      serial.port = "/dev/serial/by-id/usb-ITead_Sonoff_Zigbee_3.0_USB_Dongle_Plus_8096431ecf3aef1195062c1455516304-if00-port0";
      #specifies the MQTT broker (Mosquitto) that Zigbee2MQTT will connect to for publishing and subscribing to MQTT topics.
      mqtt.server = "mqtt://${config.userDefinedGlobalVariables.localHostIPv4}:${builtins.toString config.userDefinedGlobalVariables.servicePort.mosquitto}";  # Mosquitto MQTT broker
      frontend = {
        port = config.userDefinedGlobalVariables.servicePort.zigbee2mqttFrontend;  # Change to your desired port
      };
      advanced.log_level = "info";  # Set log level
    };
  };

  networking.firewall.allowedTCPPorts = [ config.userDefinedGlobalVariables.servicePort.zigbee2mqttFrontend ];  # Open port for Web UI
}
