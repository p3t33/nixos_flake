{ pkgs, config, ... }:
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
    ../../modules/nixos/dconf.nix
    ../../modules/nixos/networking/br0_interface.nix # used for development.
    ../../modules/nixos/networking/usb0.nix
  ];

  customOptions = {
    nvidiaHybridWithIntel = {
      nvidiaBusId = "PCI:01:00:0";
      intelBusId = "PCI:00:02:0";
    };

    systemStateVersion = "24.05";
    syncthing = {
      devicesToShareTaskWarriorFolderWith = [
        "${config.customOptions.syncthing.devices.homelab}"
        "${config.customOptions.syncthing.devices.home-desktop}"
      ];

      devicesToShareDevResourcesFolderWith = [
        "${config.customOptions.syncthing.devices.homelab}"
        "${config.customOptions.syncthing.devices.home-desktop}"
      ];
    };
  };

  # As the intel GPU in this machine is too new to be officially supported by the i915 driver
  # it needs to be forced, if this is not done the main gpu won't work and this will cause bunch
  # of bad things, including the external monitors connected via the dock not to work.
  boot.kernelParams = [
    "i915.force_probe=7d55"
  ];

  environment.systemPackages = with pkgs; [
    moolticute
    syncthing
    git-review # cli tool to interact with gerrit.
    vim
  ];
}
