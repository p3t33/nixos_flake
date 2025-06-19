{ ... }:
{

  imports = [
    ../../modules/nixos/services # imported via default.nix
  ];

  customOptions = {
    enableServicesProfile = {
      core = true;
      server = true;
    };

    enableModule = {
      wireguardQuickClient  = true;
      syncthing = true;
    };
  };
}
