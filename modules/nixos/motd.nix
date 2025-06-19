# This unit was created mostly for readability and constituency with
# the rest of the cofigurations, you can always use users.motd = <...>.
{config, lib, ...}:
let
  cfg = config.custom.motd;
in
{
  options.custom.motd = {
    enable = lib.mkEnableOption "Enable setting a Message of the Day (MOTD)";
    message = lib.mkOption {
      type = lib.types.str;
      description = "defines moto of the day";
    };
  };

  config = lib.mkIf cfg.enable {
    users.motd = cfg.message;
  };
}
