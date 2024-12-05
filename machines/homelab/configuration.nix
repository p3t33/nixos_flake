# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nicxos-help’).

{
  config,
  pkgs,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./configuration-services.nix
    ./sops-configuration.nix
    ./disko-config.nix
    ./disko-config-extra-hard-dirves.nix
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
    ../../modules/nixos/services/jellyfin.nix
    ../../modules/nixos/defaults_for_system_build.nix
    ../../modules/nixos/auto_upgrade.nix
    ../../modules/nixos/services/nginx.nix
    ../../modules/nixos/services/fail2ban.nix
    ../../modules/nixos/opengl.nix
    ../../modules/nixos/services/envfs.nix
    ../../modules/nixos/services/adguard_home.nix
    ../../modules/nixos/services/prowlarr.nix
    ../../modules/nixos/services/jackett.nix
    ../../modules/nixos/services/sonarr.nix
    ../../modules/nixos/services/radarr.nix
  ];

  systemd.tmpfiles.rules = [
    "d ${config.userDefinedGlobalVariables.pathToDataDirectory} 0770 ${config.userDefinedGlobalVariables.primeUsername} ${config.userDefinedGlobalVariables.dataGroup} -"
    "d ${config.userDefinedGlobalVariables.pathToMediaDirectory} 0770 ${config.userDefinedGlobalVariables.primeUsername} ${config.userDefinedGlobalVariables.mediaGroup} -"
    "d ${config.userDefinedGlobalVariables.pathToMediaDirectory}/tv 0770 sonarr ${config.userDefinedGlobalVariables.mediaGroup} -"
    "d ${config.userDefinedGlobalVariables.pathToMediaDirectory}/movies 0770 radarr ${config.userDefinedGlobalVariables.mediaGroup} -"
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;


  networking.interfaces.eno1.ipv4.addresses = [
    {
      address = "${config.userDefinedGlobalVariables.homeLabIP}";
      prefixLength = 24;
    }
  ];

  networking.defaultGateway = "${config.userDefinedGlobalVariables.homeLabGateway}";
  networking.nameservers = [ "8.8.8.8" ];

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Configure keymap in X11
  services.xserver = {
    xkb = {
      variant = "";
      layout = "us";
    };
  };

  users.users.${config.userDefinedGlobalVariables.primeUsername} = {

    # By default will create /etc/ssh/authorized_keys.d/$USER file with this key in it.
    # This key is added for passwordless login and this key is for VM only
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPA8Fbx91cQqosqb0P3WfOyxlCw/FMfeA7xv3uMbv4c0 work-pc"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPYfoN2VklpsgaFvSIX26ZZGbgNtri12NQVeJIVqgqTg home-desktop"
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [ ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 5201 ];
  networking.firewall.allowedUDPPorts = [ 5201 ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
}
