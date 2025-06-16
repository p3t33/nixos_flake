{ hostSpecific, ... }:
{
  virtualisation.docker.enable = true;
  users.users.${hostSpecific.primeUsername} = {
    extraGroups = [ "docker" ];
  };

}
