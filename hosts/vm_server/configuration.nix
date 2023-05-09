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
      ../../os/environment_variables.nix
      ../../os/virtualization/docker.nix
      ../../meta/meta.nix
      ../../os/users.nix
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
      hostname = "kvm-nixos-server" ;
  };


  networking.hostName = config.userDefinedGlobalVariables.hostname;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  users.users.${config.userDefinedGlobalVariables.username} = {

    # By default will create /etc/ssh/authorized_keys.d/$USER file with this key in it.
    # This key is added for passwordless login and this key is for VM only
    openssh.authorizedKeys.keys = [
     "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINs6NNbZ6EaU1x7cem1zqhDYubadH5Uww+K28e6GOmiY Motorola no password"
    ];
  };

  # Enable automatic login for the user.
  services.getty.autologinUser = config.userDefinedGlobalVariables.username;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
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
