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
      gvfs.enable = true;
      p2pool.extraArgs = [ "--mini" ];
      xmrig.numberOfThreads = 24;

      syncthing = {
        settings = {

          devices = {
            work-pc.enable = true;
            nas.enable = true;
          };

          folders = {
            taskwarrior = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.work-pc.name}"
                "${config.services.syncthing.settings.devices.nas.name}"
              ];
            };

            database = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.nas.name}"
              ];
            };

            documents = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.nas.name}"
              ];
            };

            study = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.nas.name}"
              ];
            };

            dev_resources = {
              enable = true;
              devices = [
                "${config.services.syncthing.settings.devices.work-pc.name}"
                "${config.services.syncthing.settings.devices.nas.name}"
              ];
            };
          };
        };
      };
    };
  };
}
