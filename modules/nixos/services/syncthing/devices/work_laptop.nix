{ config, ... }:
{
  services.syncthing = {
    settings = {
      devices = {
        "${config.customOptions.syncthing.devices.work-pc}" = {
          id = "Z47X7UP-AFRW6CM-UUWCAFF-A5BA6C4-PKEU7IR-XDHH6IE-N7JL54R-RTWLNAG";
          autoAcceptFolders = true;
          name = config.customOptions.syncthing.devices.work-pc;
          introducer = false;
        };
      };
    };
  };
}
