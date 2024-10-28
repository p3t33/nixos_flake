{ config, ... }:
{
  virtualisation.docker.enable = true;
  users.users.${config.userDefinedGlobalVariables.primeUsername} = {
    extraGroups = [ "docker" ];
  };

}
