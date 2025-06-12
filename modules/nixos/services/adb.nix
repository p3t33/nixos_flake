{ config, ... }:
{
  programs.adb.enable = true;
  users.users.${config.hostSpecification.primeUsername} = {
    extraGroups = [ "adbusers" ];
  };
}
