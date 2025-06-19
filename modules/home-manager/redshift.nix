{ config, lib, ... }:
{
  # Redshift is a program that adjusts the color
  # temperature of your screen.

  # it is intended to be used with xorg only.
  # -----------------------------------------


  config = lib.mkIf config.services.redshift.enable {
    services.redshift = {
      provider = "manual";
      latitude = 31.04;
      longitude = 34.85;
    };
  };
}
