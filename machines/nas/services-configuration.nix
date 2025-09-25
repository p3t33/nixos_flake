{ ... }:
{

  imports = [
    ../../modules/nixos/services # imported via default.nix
  ];


  services.zfs.autoScrub.enable = true;

  custom = {
    profiles.systemServices = {
      core.enable = true;
      server.enable = true;
    };
  };
}
