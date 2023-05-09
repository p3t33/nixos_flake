{ config, ... }:
{
      virtualisation.docker.enable = true;
      users.users.${config.userDefinedGlobalVariables.username} = {
          extraGroups = [ "docker" ];
      };

}
