{ config, ... }:
{
  services.syncthing = {
    settings = {
      devices = {
        "${config.customOptions.syncthing.devices.home-desktop}" = {
          id = "TQ34X45-BKERB7F-LBQSEGR-ZAGQITL-RL5B242-PQSTHCX-2XBQBYL-ORMFTAH";
          autoAcceptFolders = true;
          name = "${config.customOptions.syncthing.devices.home-desktop}";
          introducer = false;
        };
      };
    };
  };
}
