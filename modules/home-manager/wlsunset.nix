{ config, lib, ... }:

{
  config = lib.mkIf config.services.wlsunset.enable {
    services.wlsunset = {
      latitude = 31.04;
      longitude = 34.85;
    };
  };
}
