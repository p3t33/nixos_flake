{  config, ... }:
{
    programs.adb.enable = true;
    users.users.${config.userDefinedGlobalVariables.username} = {
        extraGroups = [ "adbusers" ];
    };
}

