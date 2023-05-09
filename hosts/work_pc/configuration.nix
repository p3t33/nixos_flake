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
      ../../meta/meta.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";


  # IMPORTANT: hostname must be defined!
  # All of the global variables are defined based on the value set for it. Many
  # files use them and by not setting the hostname they will be using thier
  # default values which may cause all kind of issues.
  userDefinedGlobalVariables = {
      enable = true;
      hostname = "HP-Zbook";
  };

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };


  # docker
  virtualisation.docker.enable = true;

  # VirtualBox
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.enableExtensionPack = true;
  virtualisation.virtualbox.guest.x11 = true;
  users.extraGroups.vboxusers.members = [ config.userDefinedGlobalVariables.username ];

  #KVM
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

  services.udev.packages = [ pkgs.moolticute.udev ];

  networking.hostName = config.userDefinedGlobalVariables.hostname;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  networking.interfaces.enp0s20f0u6u3u1.useDHCP = false;
  networking.interfaces.enp0s20f0u6u3u1.ipv4.addresses = [ {
    address = "192.168.99.1";
    prefixLength = 24;
  } ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.defaultUserShell = pkgs.zsh;
  users.users.${config.userDefinedGlobalVariables.username} = {
    isNormalUser = true;
    initialPassword = "changeme";
    description = config.userDefinedGlobalVariables.username;
    extraGroups = [ "networkmanager" "wheel" "docker" "libvirtd" ];
    packages = with pkgs; [];
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  environment.variables.EDITOR = "nvim";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    rofi-power-menu

    moolticute
    polybar

    pulseaudio
    syncthing
    nitrogen # A wallpaper browser and setter for X11
    picom # A fork of XCompMgr, a sample compositing manager for X servers
    git-review

    # Virt-manager
    virt-manager
    qemu_kvm
    qemu
    libvirt
    sqlite

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
