{ config, ... }:
{

  imports = [
    ../../modules/nixos/services # imported via default.nix
    ../../modules/nixos/custom-global-options/networking.nix
  ];

  services.syncthing.enable = true;
  services.trezord.enable = true;

  custom = {
    profiles.systemServices = {
      core.enable = true;
      desktop.enable = true;
      xmr-miner.enable = true;
    };

    services.p2pool.extraArgs = [ "--mini" ];

    services.syncthing = {

      remoteDevices = {
        homelab.enable = true;
        work-pc.enable = true;
      };

      foldersToShare = {

        taskwarrior = {
          enable = true;
          devicesToShareWith = [
            "${config.custom.services.syncthing.remoteDevices.homelab.name}"
            "${config.custom.services.syncthing.remoteDevices.work-pc.name}"
          ];
        };

        database = {
          enable = true;
          devicesToShareWith = [
            "${config.custom.services.syncthing.remoteDevices.homelab.name}"
          ];
        };

        documents = {
          enable = true;
          devicesToShareWith = [
            "${config.custom.services.syncthing.remoteDevices.homelab.name}"
          ];
        };

        study = {
          enable = true;
          devicesToShareWith = [
            "${config.custom.services.syncthing.remoteDevices.homelab.name}"
          ];
        };

        devResources = {
          enable = true;
          devicesToShareWith = [
            "${config.custom.services.syncthing.remoteDevices.homelab.name}"
            "${config.custom.services.syncthing.remoteDevices.work-pc.name}"
          ];
        };

      };

    };
  };
}
