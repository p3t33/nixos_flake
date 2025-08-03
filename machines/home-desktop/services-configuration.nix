{ config, ... }:
{

  imports = [
    ../../modules/nixos/services # imported via default.nix
    ../../modules/nixos/custom-global-options/networking.nix
  ];

  services = {
    syncthing.enable = true;
    trezord.enable = true;
  };

  custom = {
    profiles.systemServices = {
      core.enable = true;
      desktop.enable = true;
      xmr-miner.enable = true;
    };

    services = {
      p2pool.extraArgs = [ "--mini" ];
      xmrig.numberOfThreads = 24;

      syncthing = {
        settings = {

          devices = {
            homelab.enable = true;
            work-pc.enable = true;
          };

          folders = {
            taskwarrior = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.homelab.name}"
                "${config.services.syncthing.settings.devices.work-pc.name}"
              ];
            };

            database = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.homelab.name}"
              ];
            };

            documents = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.homelab.name}"
              ];
            };

            study = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.homelab.name}"
              ];
            };

            dev_resources = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.homelab.name}"
                "${config.services.syncthing.settings.devices.work-pc.name}"
              ];
            };
          };
        };
      };
    };
  };
}
