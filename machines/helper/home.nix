{ inputs, ... }:
{
  imports = [
    ./sops-home.nix
    inputs.self.modules.homeManager.profile-home-manager # imported via default.nix
  ];

  programs.antigravity-cli.enable = true;

  custom = {
    profiles.homeManager = {
      core.enable = true;
    };
  };
}
