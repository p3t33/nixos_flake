{ ... }:
{
  services.home-assistant.enable = true;
  services.mosquitto.enable = true;
  services.zigbee2mqtt.enable = true;
  services.zfs.autoScrub.enable = true;

  custom = {
    profiles.systemServices = {
      core.enable = true;
      server.enable = true;
    };
  };
}
