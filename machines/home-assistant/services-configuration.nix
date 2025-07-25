{ ... }:
{

  imports = [
    ../../modules/nixos/services # imported via default.nix
  ];

  customOptions = {
    enableServicesProfile = {
      core = true;
      server = true;
      xmr-miner = true;
    };

    enableModule = {
      homeAssistant = true;
      mosquitto = true;
      zigbee2mqtt = true;
      zfsAutoScrub  = true;
    };
  };
}
