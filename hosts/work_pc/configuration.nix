# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nicxos-help’).

{ config, pkgs, ... }:

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
      ../../os/environment_variables.nix
      ../../os/virtualization/docker.nix
      ../../os/virtualization/kvm.nix
      ../../os/virtualization/virtualbox.nix
      ../../os/users.nix
      ../../os/gui.nix
      ../../os/dictionaries.nix
      ../../meta/meta.nix
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
  # files use them and by not setting the hostname they will be using thier
  # default values which may cause all kind of issues.
  userDefinedGlobalVariables.hostname = "HP-Zbook";
  networking.hostName = config.userDefinedGlobalVariables.hostname;





  programs.dconf.enable = true;

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

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
