{ config, ... }:
{
  services.syncthing = {
    settings = {
      devices = {
        "${config.customOptions.syncthing.devices.homelab}" = {
          id = "XPCO572-XPKAN7M-BXTAVRT-2WZGOBR-QWMG6F7-3EHJ276-GUZJ2UW-ZVLRCQK";
          autoAcceptFolders = true;
          name = "${config.customOptions.syncthing.devices.homelab}";
          introducer = false;
        };
      };
    };
  };
}
