{ config, lib, ... }:
{

  config = lib.mkIf config.services.home-assistant.enable {
    services.postgresql.enable = true;

    services.home-assistant = {
    # Home Assistant runs in an isolated NixOS service environment, so system-wide
    # Python packages are not available to it. We must explicitly declare additional
    # dependencies here to ensure they are available in Home Assistant's runtime.
    #
    # whitout this list I was getting erros with journalctl -u home-assistant -f
    # without numpy I was stuck on the "loading data" welcome screen.
    #
    # In normal installtoin home-assistant uses pip in run time to install its
    # dependencies into its own environment. So it is a good idea to look
    # at the loogs sometimes to see if anythiung needs to be added to the list.
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
    ] ++ lib.optional config.services.mosquitto.enable "mqtt";
    config = {
      homeassistant = {
        name = "Home";
        unit_system = "metric";
        time_zone = "${config.time.timeZone}";
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
    };
  };
 };
}
