{ config, lib, ... }:

let
  cfg = config.custom.connectivity.bluetooth;
in
{
  options.custom.connectivity.bluetooth.enable = lib.mkEnableOption "Enable Bluetooth support and Blueman applet";

  config = lib.mkIf cfg.enable {
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;
  };
}
