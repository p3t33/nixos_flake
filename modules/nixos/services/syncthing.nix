{ config, ... }:
{
  services.syncthing = {
    enable = true;
    group = "${config.userDefinedGlobalVariables.dataGroup}";
  };
}
