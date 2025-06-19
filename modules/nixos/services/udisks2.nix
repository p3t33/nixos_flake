{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.udisks2;
in
{
  options.customOptions.enableModule.udisks2 = lib.mkEnableOption "Enable udisks2 service for user-level device automounting";

  config = lib.mkIf cfg {
    services.udisks2.enable = true;
  };
}
