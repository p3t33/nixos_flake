{ config, lib, machineName, ... }:
{
  services.syncthing = {
    settings = {
        folders = {
            "database" = {
                id = "database";
                path = "${config.userDefinedGlobalVariables.syncthingSyncDir}/database";
                devices = []
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


