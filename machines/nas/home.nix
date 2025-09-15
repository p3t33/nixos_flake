{
  imports = [
    ./sops-home.nix
    ../../modules/home-manager # imported via default.nix
  ];

  custom = {
    profiles.homeManager = {
      core.enable = true;
    };
  };
}
