{
  imports = [
    ./sops-home.nix
    ../../modules/home-manager # imported via default.nix
  ];

  programs.antigravity-cli.enable = true;

  custom = {
    programs.openclaw.enable = true;

    profiles.homeManager = {
      core.enable = true;
    };
  };
}
