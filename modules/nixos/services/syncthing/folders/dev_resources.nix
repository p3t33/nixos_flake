{
  config,
  lib,
  machineName,
  ...
}:
{
  services.syncthing = {
    settings = {
      folders = {
        "dev_resources" = {
          id = "dev_resources";
          path = "${config.userDefinedGlobalVariables.syncthingSyncDir}/dev_resources";
          devices =
            [ ]
            ++ lib.optionals (machineName == "${config.userDefinedGlobalVariables.machines.work-pc}") [
              "${config.userDefinedGlobalVariables.machines.homelab}"
              "${config.userDefinedGlobalVariables.machines.home-desktop}"
            ]
            ++ lib.optionals (machineName == "${config.userDefinedGlobalVariables.machines.home-desktop}") [
              "${config.userDefinedGlobalVariables.machines.work-pc}"
              "${config.userDefinedGlobalVariables.machines.homelab}"
            ]
            ++ lib.optionals (machineName == "${config.userDefinedGlobalVariables.machines.homelab}") [
              "${config.userDefinedGlobalVariables.machines.work-pc}"
              "${config.userDefinedGlobalVariables.machines.home-desktop}"
            ];
        };
      };
    };
  };
}
