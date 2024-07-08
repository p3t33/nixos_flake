# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nicxos-help’).

{ inputs, config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./configuration-services.nix
      ./disko-config.nix
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
      ../../modules/nixos/environment_variables.nix
      ../../modules/nixos/virtualization/docker.nix
      ../../modules/nixos/networking/networkmanager.nix
      ../../modules/nixos/networking/hostname.nix
      ../../modules/nixos/users.nix
      ../../modules/nixos/dictionaries.nix
      ../../modules/nixos/command_not_found.nix # needs to be set to false as it is mutually exclusive with nix-index
      ../../modules/nixos/defaults_for_system_build.nix
      ../../modules/nixos/opengl.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  users.users.${config.userDefinedGlobalVariables.primeUsername} = {

    # By default will create /etc/ssh/authorized_keys.d/$USER file with this key in it.
    # This key is added for passwordless login and this key is for VM only
    openssh.authorizedKeys.keys = [
     "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINs6NNbZ6EaU1x7cem1zqhDYubadH5Uww+K28e6GOmiY Motorola no password"
    ];
  };

  # Enable automatic login for the user.
  services.getty.autologinUser = config.userDefinedGlobalVariables.primeUsername;

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
