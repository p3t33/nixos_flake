{
  pkgs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ./services-configuration.nix
    ./sops-configuration.nix
    ./disko-configuration.nix
    ../../modules/nixos # imported via default.nix
  ];

  custom = {
    profiles.system = {
      core.enable = true;
      desktop.enable = true;
      securityKeys.enable = true;
      virtualization.enable = true;
      gaming.enable = true;
    };

    systemStateVersion = "24.05";
  };

  environment.systemPackages = with pkgs; [

    signal-desktop
    moolticute

    syncthing
    git-review
    ntfs3g
    calibre
    alacritty
  ];
}
