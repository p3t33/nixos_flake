{ config, ... }: {
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
    };


    wallpaperName = "watchtower.png";
    sshPublicKey = config.customGlobalOptions.sshPublicKeys.work-pc.key;
  };
}
