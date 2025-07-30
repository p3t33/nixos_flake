{ ... }:
{

  imports = [
    ../../modules/nixos/services # imported via default.nix
  ];

  custom = {
    profiles.systemServices = {
      core = true;
      server = true;
    };
  };
}
