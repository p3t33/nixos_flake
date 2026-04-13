{
  imports = [
    ./sops-home.nix
    ../../modules/home-manager # imported via default.nix
  ];

  custom = {
    programs.openclaw.enable = true;

    profiles.homeManager = {
      core.enable = true;
    };
  };
}
