{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.bluetooth;
in
{
  options.customOptions.enableModule.bluetooth = lib.mkEnableOption "Enable Bluetooth support and Blueman applet";

  config = lib.mkIf cfg {
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;
  };
}
