{ config, ... }:
{
  imports = [
    ./sops-home.nix
    ../../modules/home-manager # imported via default.nix
  ];

  programs.gpg.enable = true;

  custom = {
    programs.gitCommitSignaturesWithGpg.enable = true;

    profiles.homeManager = {
      core = true;
      desktop = true;
    };

    file.smartcardPublicKey = {
      enable = true;
      value = config.customGlobal.sshPublicKeys.home-desktop.key;
    };

    desktop.wallpaper.name = "crane_at_night.png";
  };
}
