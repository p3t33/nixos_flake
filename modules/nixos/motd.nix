{config, lib, ...}:
{
  options.customOptions = {
      motd = lib.mkOption {
      type = lib.types.str;
      description = "defines moto of the day";
    };
  };

  config = {
    users.motd = config.customOptions.motd;
  };
}
