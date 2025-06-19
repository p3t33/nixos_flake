{ config, lib, hostSpecific, ... }:
{
  config = lib.mkIf config.virtualisation.docker.enable {
    users.users.${hostSpecific.primeUsername} = {
      extraGroups = [ "docker" ];
    };
  };
}
