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
        "study" = {
          id = "study";
          path = "${config.userDefinedGlobalVariables.syncthingSyncDir}/study";
          devices =
            [ ]
            ++ lib.optionals (machineName == "${config.userDefinedGlobalVariables.machines.home-desktop}") [
              "${config.userDefinedGlobalVariables.machines.homelab}"
            ]
            ++ lib.optionals (machineName == "${config.userDefinedGlobalVariables.machines.homelab}") [
              "${config.userDefinedGlobalVariables.machines.home-desktop}"
            ];
        };
      };
    };
  };
}
