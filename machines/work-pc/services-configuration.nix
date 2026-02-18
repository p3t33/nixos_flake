{ pkgs, config, ... }:
{
  virtualisation.virtualbox.host.enable = true;
  services = {
    fwupd.enable = true;
    syncthing.enable = true;
    ollama = {
      enable = true;
      package = pkgs.ollama-cuda;
    };
  };

  programs.adb.enable = true;

  custom = {
    profiles.systemServices = {
      core.enable = true;
      desktop.enable = true;
    };

    connectivity.bluetooth.enable = true;

    services.syncthing = {
      settings = {
        devices = {
          home-desktop.enable = true;
          nas.enable = true;
        };

        folders = {
          taskwarrior = {
            enable = true;
            devices = [
              "${config.services.syncthing.settings.devices.home-desktop.name}"
              "${config.services.syncthing.settings.devices.nas.name}"
            ];
          };

          dev_resources = {
            enable = true;
            devices = [
              "${config.services.syncthing.settings.devices.home-desktop.name}"
              "${config.services.syncthing.settings.devices.nas.name}"
            ];
          };
        };
      };
    };
  };
}
