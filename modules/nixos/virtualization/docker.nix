{ config, ... }:
{
  virtualisation.docker.enable = true;
  users.users.${config.hostSpecification.primeUsername} = {
    extraGroups = [ "docker" ];
  };

}
