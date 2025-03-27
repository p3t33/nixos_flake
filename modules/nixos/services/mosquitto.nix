{pkgs, config, ... }:
{
  environment.systemPackages = with pkgs; [
    mosquitto  # Installs Mosquitto and its CLI tools (mosquitto_pub & mosquitto_sub)
  ];

  # MQTT message broker, to test:
  # mosquitto_sub -t "test"
  #
  # mosquitto_pub -m "message from mosquitto_pub client" -t "test"
  services.mosquitto = {
    enable = true;
    listeners = [
      {
        address = "${config.userDefinedGlobalVariables.anyIPv4}";  # Listen on all interfaces
        port = config.userDefinedGlobalVariables.servicePort.mosquitto;  # Explicitly set the MQTT port
        acl = [ "pattern readwrite #" ];
        omitPasswordAuth = true;
        settings.allow_anonymous = true;
      }
    ];
  };

}

