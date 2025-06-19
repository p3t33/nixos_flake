{pkgs, config, lib, ... }:
{
  # Created so I can share it with better access then the one form the listeners set.
  options.custom =
  {
    servicePort.mosquitto = lib.mkOption {
      type = lib.types.int;
      default = 1883;
      description = "mosquitto port";
    };
  };

  config = lib.mkIf config.services.mosquitto.enable {
    environment.systemPackages = with pkgs; [
      mosquitto  # Installs Mosquitto and its CLI tools (mosquitto_pub & mosquitto_sub)
    ];

    # MQTT message broker, to test:
    # mosquitto_sub -t "test"
    #
    # mosquitto_pub -m "message from mosquitto_pub client" -t "test"
    services.mosquitto = {
      listeners = [
        {
          address = "${config.customGlobal.anyIPv4}";  # Listen on all interfaces
          port = config.custom.servicePort.mosquitto;  # Explicitly set the MQTT port
          acl = [ "pattern readwrite #" ];
          omitPasswordAuth = true;
          settings.allow_anonymous = true;
        }
      ];
    };
  };
}

