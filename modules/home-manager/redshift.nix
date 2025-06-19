{ config, lib, ... }:
let
  cfg = config.customOptions.enableModule.redshift;
in
{
  # Redshift is a program that adjusts the color
  # temperature of your screen.

  # it is intended to be used with xorg only.
  # -----------------------------------------

  options.customOptions.enableModule.redshift = lib.mkEnableOption "Whether to enable Redshift screen temperature adjustment";

  config = lib.mkIf cfg {
    services.redshift = {
      enable = true;
      provider = "manual";
      latitude = 31.04;
      longitude = 34.85;
    };
  };
}
