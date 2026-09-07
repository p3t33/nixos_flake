{ config, ... }:
let
  gpt-reasoning = "gpt-5.6-sol";
  gpt-workhorse = "gpt-5.6-terra";
in
{
  imports = [
    ./sops-home.nix
    ../../modules/home-manager # imported via default.nix
  ];

  programs.antigravity-cli.enable = true;
  programs.gpg.enable = true;

  custom = {
    programs = {
      pi = {
        defaultProvider = "github-copilot";
        defaultModel = gpt-reasoning;
        models = {
          reasoning = "${config.custom.programs.pi.defaultProvider}/${gpt-reasoning}";
          workhorse = "${config.custom.programs.pi.defaultProvider}/${gpt-workhorse}";
        };
        defaultThinkingLevel = "medium";
      };
      gitCommitSignaturesWithGpg.enable = true;
      glabels.enable = true;
    };

    profiles.homeManager = {
      core.enable = true;
      desktop.enable = true;
    };

    file.smartcardPublicKey = {
      enable = true;
      value = config.custom.shared.sshPublicKeys.work-pc.key;
    };

    desktop.wallpaper.name = "watchtower.png";

    polybar = {
      enableWlan = true;
      enableBattery = true;
      enableAllenTxTime = true;
    };
  };
}
