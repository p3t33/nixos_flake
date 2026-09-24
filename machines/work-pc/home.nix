{ config, ... }:
let
  gpt-reasoning = "gpt-6-sol";
  gpt-workhorse = "gpt-6-luna";
in
{
  imports = [
    ./sops-home.nix
    ../../modules/home-manager # imported via default.nix
  ];

  programs.antigravity-cli.enable = true;
  programs.gpg.enable = true;
  programs.wezterm.enable = true;

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
      desktop = {
        common.enable = true;
        wayland.enable = true;
      };
    };

    scripts.displayRecovery = {
      enable = true;
      builtInOutput = "eDP-1";
      expectedExternalDisplays = 2;
    };

    file.smartcardPublicKey = {
      enable = true;
      value = config.custom.shared.sshPublicKeys.work-pc.key;
    };

    desktop.wallpaper.name = "watchtower.png";

    waybar = {
      enableWlan = true;
      enableBattery = true;
      enableAllenTxTime = true;
    };
  };

  wayland.windowManager.sway.config.output."eDP-1".enable = "";
}
