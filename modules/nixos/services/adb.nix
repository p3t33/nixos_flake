{ config, lib, hostSpecific, ... }:
{
  config = lib.mkIf config.programs.adb.enable {
    users.users.${hostSpecific.primeUsername} = {
      extraGroups = [ "adbusers" ];
    };
  };
}
