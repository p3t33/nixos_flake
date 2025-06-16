{pkgs, config, lib, ... }:
{
  # Created so I can share it with better access then the one form the listeners set.
  options.customOptions.servicePort.mosquitto = lib.mkOption {
    type = lib.types.int;
    default = 1883;
    description = "mosquitto port";
  };

  config = {
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
          address = "${config.customGlobalOptions.anyIPv4}";  # Listen on all interfaces
          port = config.customOptions.servicePort.mosquitto;  # Explicitly set the MQTT port
          acl = [ "pattern readwrite #" ];
          omitPasswordAuth = true;
          settings.allow_anonymous = true;
        }
      ];
    };
  };
}

