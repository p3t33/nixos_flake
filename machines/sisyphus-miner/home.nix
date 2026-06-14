{
  imports = [
    ./sops-home.nix
    ../../modules/home-manager # imported via default.nix
  ];

  programs.antigravity-cli.enable = true;

  custom = {
    profiles.homeManager = {
      core.enable = true;
    };
  };
}
