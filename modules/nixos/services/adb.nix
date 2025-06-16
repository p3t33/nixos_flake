{ hostSpecific, ... }:
{
  programs.adb.enable = true;
  users.users.${hostSpecific.primeUsername} = {
    extraGroups = [ "adbusers" ];
  };
}
