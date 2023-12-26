# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nicxos-help’).

{ config, machineName, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./configuration-services.nix
      ./sops-configuration.nix
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
      ../../modules/nixos/system_packages/iac.nix
      ../../modules/nixos/sound.nix
      ../../modules/nixos/bluetooth.nix
      ../../modules/nixos/networking.nix
      ../../modules/nixos/environment_variables.nix
      ../../modules/nixos/virtualization/docker.nix
      ../../modules/nixos/virtualization/kvm.nix
      ../../modules/nixos/virtualization/virtualbox.nix
      ../../modules/nixos/users.nix
      ../../modules/nixos/gui.nix
      ../../modules/nixos/dictionaries.nix
      ../../meta/meta.nix
      ../../modules/nixos/security/nitrokey.nix
      ../../modules/nixos/services/adb.nix
      ../../modules/nixos/command_not_found.nix # needs to be set to false as it is mutually exclusive with nix-index
    ];


  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";


  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  networking.hostName = machineName;

  # Nvidia PRIME(technology used to manage hybrid graphics) settings
  # Note: non hybrid Nvidia graphics have a bit different configurations.
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.prime = {

    # In this mode the Nvidia card is only activated on demand
    # There is also the sync mode in which In this mode the Nvidia card is
    # turned on constantly, having impact on laptop battery and health.
    # And in this mode there might be some issues.
    # In any case it is always a good idia to keep an eye on the official documentation.
    offload = {
      enable = true;
      enableOffloadCmd = true;
    };

    # found by executing lspci | grep -E 'VGA|3D'
    nvidiaBusId = "PCI:01:00:0";
    intelBusId = "PCI:00:02:0";
  };


  programs.dconf.enable = true;

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    moolticute
    syncthing
    git-review # cli tool to interact with gerrit.
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;


  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
}
