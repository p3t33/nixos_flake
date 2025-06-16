{config, ... }:
{
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
    ];

   openFirewall = true;
    enable = true;
    # only install components but does not enable them.
    extraComponents = [ "mqtt" ];  # Enables MQTT integration to use the MQTT broker provided by services.mosquitto.
    config = {
      homeassistant = {
        name = "Home";
        unit_system = "metric";
        time_zone = "${config.time.timeZone}";
        temperature_unit = "C";
      };
      http = {
        server_host = "${config.customGlobalOptions.anyIPv4}";
        server_port = 8123;
      };
    };
  };
}
