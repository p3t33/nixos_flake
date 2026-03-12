{ config, lib, ... }:
{
  imports = [
    ./schema.nix
    ./automations.nix
    ./lovelace.nix
  ];

  config = lib.mkIf config.services.home-assistant.enable {

    services.postgresql.enable = true;

    services.home-assistant = {
      # Home Assistant runs in an isolated NixOS service environment, so system-wide
      # Python packages are not available to it. We must explicitly declare additional
      # dependencies here to ensure they are available in Home Assistant's runtime.
      #
      # Without numpy Home Assistant is stuck on the "loading data" welcome screen.
      # Check journalctl -u home-assistant -f for missing packages over time.
      extraPackages = python3Packages: with python3Packages; [
        numpy
        zlib-ng
        aiohttp-fast-zlib
        isal
        psycopg2
      ];

      openFirewall = true;
      extraComponents = [
        "sun"          # sunrise/sunset, used by automations
        "met"          # free weather forecast, no API key needed
        "mobile_app"   # companion app on your phone
        "isal"         # compression performance improvement
        "camera"       # core component — pulls in pyturbojpeg
        "conversation" # core component — pulls in hassil + home-assistant-intents
        "roomba"       # iRobot Roomba vacuum
      ] ++ lib.optional config.services.mosquitto.enable "mqtt";

      config = {
        homeassistant = {
          name             = "Home";
          unit_system      = "metric";
          time_zone        = "${config.time.timeZone}";
          temperature_unit = "C";
        };
        http = {
          server_host = "${config.custom.shared.anyIPv4}";
          server_port = 8123;
        };
        # Use PostgreSQL instead of the default SQLite for better performance
        # and reliability with large history datasets.
        # Connects via Unix socket using peer auth (hass user → hass db).
        recorder.db_url = "postgresql://@/hass";
        # history depends on recorder and enables the History panel
        # and statistics-graph cards in Lovelace
        history = {};
        # Required to load the mobile_app domain — extraComponents installs
        # the Python deps but this entry enables it in configuration.yaml
        mobile_app = {};
      };
    };
  };
}
