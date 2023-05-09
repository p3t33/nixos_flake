{ config, ... }:
{
      virtualisation.libvirtd.enable = true;
        users.users.${config.userDefinedGlobalVariables.username} = {
            extraGroups = [ "libvirtd" ];
  };

}
