{ config, lib, machineName, ... }:
{
  services.syncthing = {
    settings = {
        folders = {
            "documents" = {
                id = "documents";
                path = "${config.userDefinedGlobalVariables.syncthingSyncDir}/documents";
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


