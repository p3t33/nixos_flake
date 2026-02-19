{ pkgs, config, ... }:
{
  services = {
    syncthing.enable = true;
    trezord.enable = true;
    ollama = {
      enable = true;
      package = pkgs.ollama-cpu;
    };
  };

  custom = {
    profiles.systemServices = {
      core.enable = true;
      desktop.enable = true;
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
