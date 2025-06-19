{ config, lib, hostSpecific, ... }:

let
  cfg = config.customOptions.enableModule.docker;
in
{
  options.customOptions.enableModule.docker = lib.mkEnableOption "Enable Docker and add user to the docker group";

  config = lib.mkIf cfg {
    virtualisation.docker.enable = true;
    users.users.${hostSpecific.primeUsername} = {
      extraGroups = [ "docker" ];
    };
  };
}
