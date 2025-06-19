{ config, lib, ... }:

{
  config = lib.mkIf config.services.picom.enable {
    services.picom = {
      backend = "xrender";
    };
  };
}
