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
      ../../os/users.nix
      ../../os/gui.nix
      ../../meta/meta.nix
    ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;



  # IMPORTANT: hostname must be defined!
  # All of the global variables are defined based on the value set for it. Many
  # files use them and by not setting the hostname they will be using thier
  # default values which may cause all kind of issues.
  userDefinedGlobalVariables = {
      enable = true;
      hostname = "home-desktop";
  };

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable grub cryptodisk
  boot.loader.grub.enableCryptodisk=true;

  boot.initrd.luks.devices."luks-01b11c05-22ec-4569-b265-29e1ca00b72b".keyFile = "/crypto_keyfile.bin";
  # Enable swap on luks
  boot.initrd.luks.devices."luks-d1cf13c1-d395-469b-8c26-651828d85993".device = "/dev/disk/by-uuid/d1cf13c1-d395-469b-8c26-651828d85993";
  boot.initrd.luks.devices."luks-d1cf13c1-d395-469b-8c26-651828d85993".keyFile = "/crypto_keyfile.bin";

  programs.dconf.enable = true;

  networking.hostName = config.userDefinedGlobalVariables.hostname;
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

    signal-desktop
    moolticute

    syncthing
    git-review
    ntfs3g
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
}
