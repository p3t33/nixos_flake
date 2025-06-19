{ config, ... }:
{
  imports = [
    ./sops-home.nix
    ../../modules/home-manager # imported via default.nix
  ];

  customOptions = {
    enableHomeManagerProfile = {
      core = true;
      desktop = true;
    };

    enableModule = {
      gitCommitSignaturesWithGpg = true;
      gpgAgent = true;
      enableSmartcardPublicKey  = true;
      taskwarrior = true;
    };

    wallpaperName = "crane_at_night.png";
    sshPublicKey = config.customGlobalOptions.sshPublicKeys.home-desktop.key;
  };
}
