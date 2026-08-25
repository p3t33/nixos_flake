{ ... }:
{
  services.home-assistant = {
    enable = true;
    config.http = {
      use_x_forwarded_for = true;
      trusted_proxies = [ "192.168.1.20" ];
    };
  };
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
