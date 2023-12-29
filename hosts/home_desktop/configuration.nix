# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nicxos-help’).

{ inputs, machineName, config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./configuration-services.nix
      ./sops-configuration.nix
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
      ../../modules/nixos/system_packages/iac.nix
      ../../modules/nixos/sound.nix
      ../../modules/nixos/networking.nix
      ../../modules/nixos/environment_variables.nix
      ../../modules/nixos/virtualization/docker.nix
      ../../modules/nixos/virtualization/kvm.nix
      ../../modules/nixos/users.nix
      ../../modules/nixos/gui.nix
      ../../modules/nixos/dictionaries.nix
      ../../modules/nixos/security/nitrokey.nix
      ../../modules/nixos/command_not_found.nix # needs to be set to false as it is mutually exclusive with nix-index
      ../../modules/nixos/defaults_for_system_build.nix
    ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;
  boot.initrd.kernelModules = [ "amdgpu" ];


  # IMPORTANT: hostname must be defined!
  # All of the global variables are defined based on the value set for it. Many
  # files use them and by not setting the hostname they will be using thier
  # default values which may cause all kind of issues.
  networking.hostName = machineName;


  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable grub cryptodisk
  boot.loader.grub.enableCryptodisk=true;

  boot.initrd.luks.devices."luks-663db9ae-4317-4f9d-8860-1414b4ef27ed".keyFile = "/crypto_keyfile.bin";

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

    signal-desktop
    moolticute

    syncthing
    git-review
    ntfs3g
    calibre

    mesa.drivers # For Vulkan support
    rocm-opencl-icd # For OpenCL support
  ];

  services.xserver = {
  enable = true;
  videoDrivers = [ "amdgpu" ]; # Enables the AMDGPU driver
};

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
