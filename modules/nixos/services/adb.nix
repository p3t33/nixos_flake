{ config, ... }:
{
  programs.adb.enable = true;
  users.users.${config.userDefinedGlobalVariables.primeUsername} = {
    extraGroups = [ "adbusers" ];
  };
}
