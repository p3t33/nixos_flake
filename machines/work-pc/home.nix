{ config, ... }:
let
  claude-opus = "claude-opus-5";
  claude-sonnet = "claude-sonnet-5";
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
        defaultModel = claude-opus;
        models = {
          reasoning = "${config.custom.programs.pi.defaultProvider}/${claude-opus}";
          workhorse = "${config.custom.programs.pi.defaultProvider}/${claude-sonnet}";
        };
        defaultThinkingLevel = "medium";
      };
      gitCommitSignaturesWithGpg.enable = true;
    };

    profiles.homeManager = {
      core.enable = true;
      desktop.enable = true;
      ai.enable = true;
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
