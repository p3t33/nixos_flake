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
        defaultModel = gpt-reasoning;
        models = {
          reasoning = "${config.custom.programs.pi.defaultProvider}/${gpt-reasoning}";
          workhorse = "${config.custom.programs.pi.defaultProvider}/${gpt-workhorse}";
        };
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

    file.smartcardPublicKey = {
      enable = true;
      value = config.custom.shared.sshPublicKeys.home-desktop.key;
    };

    desktop.wallpaper.name = "crane_at_night.png";
  };
}
