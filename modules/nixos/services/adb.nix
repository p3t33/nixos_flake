{ config, lib, hostSpecific, ... }:

let
  cfg = config.customOptions.enableModule.adb;
in
{
  options.customOptions.enableModule.adb = lib.mkEnableOption "Enable Android Debug Bridge (ADB) support";

  config = lib.mkIf cfg {
    programs.adb.enable = true;
    users.users.${hostSpecific.primeUsername} = {
      extraGroups = [ "adbusers" ];
    };
  };
}
