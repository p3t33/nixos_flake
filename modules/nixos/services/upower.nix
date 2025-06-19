{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.upower;
in
{
  options.customOptions.enableModule.upower = lib.mkEnableOption "Enable UPower for power management support";

  # used by polybar battery module.
  config = lib.mkIf cfg {
    services.upower.enable = true;
  };
}
