{config, lib, ...}:
let
  cfg = config.customOptions.enableModule.motd;
in
{
  options.customOptions = {
    enableModule.motd = lib.mkEnableOption "Enable setting a Message of the Day (MOTD)";
    motd = lib.mkOption {
      type = lib.types.str;
      description = "defines moto of the day";
    };
  };

  config = lib.mkIf cfg {
    users.motd = config.customOptions.motd;
  };
}
