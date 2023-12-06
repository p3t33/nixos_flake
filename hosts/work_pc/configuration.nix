# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nicxos-help’).

{ inputs, config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./configuration-services.nix
      ../../os/fonts.nix
      ../../os/experimental-features.nix
      ../../os/garbage_collection.nix
      ../../os/system_version.nix
      ../../os/non_free_software.nix
      ../../os/locale.nix
      ../../os/system_packages/development.nix
      ../../os/system_packages/cli_utilities.nix
      ../../os/system_packages/encryption.nix
      ../../os/system_packages/gui.nix
      ../../os/system_packages/iac.nix
      ../../os/sound.nix
      ../../os/bluetooth.nix
      ../../os/networking.nix
      ../../os/environment_variables.nix
      ../../os/virtualization/docker.nix
      ../../os/virtualization/kvm.nix
      ../../os/virtualization/virtualbox.nix
      ../../os/users.nix
      ../../os/gui.nix
      ../../os/dictionaries.nix
      ../../meta/meta.nix
      ../../os/security/nitrokey.nix
      ../../os/services/adb.nix
      ../../os/command_not_found.nix # needs to be set to false as it is mutually exclusive with nix-index
      ../../home/home-manager-configuration.nix
    ];


  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";


  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # IMPORTANT: hostname must be defined!
  # All of the global variables are defined based on the value set for it. Many
  # files use them and by not setting the hostname they will be using their
  # default values which may cause all kind of issues.
  userDefinedGlobalVariables.hostname = "HP-Zbook";
  networking.hostName = config.userDefinedGlobalVariables.hostname;

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
