{ ... }:
{
  # Redshift is a program that adjusts the color 
  # temperature of your screen.

  # it is intended to be used with xorg only. 
  # -----------------------------------------

  services.redshift = {
    enable = true;
    provider = "manual";
    latitude = 31.04;
    longitude = 34.85;
  };
}
