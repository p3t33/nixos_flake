{
  pkgs,
  config,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ./configuration-services.nix
    ./sops-configuration.nix
    ./disko-config.nix
    ../../modules/nixos/bootloader/systemd-boot.nix
    ../../modules/meta.nix
    ../../modules/nixos/home-manager-as-nixos-module.nix
    ../../modules/nixos/fonts.nix
    ../../modules/nixos/experimental-features.nix
    ../../modules/nixos/garbage_collection.nix
    ../../modules/nixos/system_version.nix
    ../../modules/nixos/non_free_software.nix
    ../../modules/nixos/locale.nix
    ../../modules/nixos/system_packages/development.nix
    ../../modules/nixos/system_packages/cli_utilities.nix
    ../../modules/nixos/system_packages/encryption.nix
    ../../modules/nixos/system_packages/gui.nix
    ../../modules/nixos/networking/networkmanager.nix
    ../../modules/nixos/networking/hostname.nix
    ../../modules/nixos/environment_variables.nix
    ../../modules/nixos/virtualization/docker.nix
    ../../modules/nixos/virtualization/kvm.nix
    ../../modules/nixos/users.nix
    ../../modules/nixos/desktop_environment.nix
    ../../modules/nixos/dictionaries.nix
    ../../modules/nixos/security/nitrokey.nix
    ../../modules/nixos/security/solokey2.nix
    ../../modules/nixos/defaults_for_system_build.nix
    ../../modules/nixos/graphics.nix
    ../../modules/nixos/gaming/steam.nix
    ../../modules/nixos/dconf.nix
  ];

  customOptions = {
    systemStateVersion = "24.05";
    syncthing = {
      devicesToShareTaskWarriorFolderWith = [
        "${config.customOptions.syncthing.devices.homelab}"
        "${config.customOptions.syncthing.devices.work-pc}"
      ];
      devicesToShareDevResourcesFolderWith = [
        "${config.customOptions.syncthing.devices.homelab}"
        "${config.customOptions.syncthing.devices.work-pc}"
      ];
      devicesToShareDatabaseFolderWith = [
        "${config.customOptions.syncthing.devices.homelab}"
      ];
      devicesToShareDocumentsFolderWith = [
        "${config.customOptions.syncthing.devices.homelab}"
      ];
      devicesToShareStudyFolderWith = [
        "${config.customOptions.syncthing.devices.homelab}"
      ];
    };
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
