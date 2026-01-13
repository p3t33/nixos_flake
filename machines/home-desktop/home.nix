{ config, ... }:
{
  imports = [
    ./sops-home.nix
    ../../modules/home-manager # imported via default.nix
  ];

  programs.gpg.enable = true;

  custom = {
    programs.gitCommitSignaturesWithGpg.enable = true;
    programs.aichat.enable = true;

    profiles.homeManager = {
      core.enable = true;
      desktop.enable = true;
    };

    file.smartcardPublicKey = {
      enable = true;
      value = config.customGlobal.sshPublicKeys.home-desktop.key;
    };

    desktop.wallpaper.name = "crane_at_night.png";
  };
}
