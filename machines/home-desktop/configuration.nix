{
  pkgs,
  config,
  machineName,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ./configuration-services.nix
    ./sops-configuration.nix
    ./disko-config.nix
    ../../modules/nixos/bootloader/systemd-boot.nix
    ../../modules/meta.nix
    ../../modules/common/host-specification.nix
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
    ../../modules/nixos/gaming/steam.nix
    ../../modules/nixos/dconf.nix
  ];

  hostSpecification = {
    primeUsername = "kmedrish";
    hostConfigurationName = machineName;
    wallpaperName = "crane_at_night.png";
    systemStateVersion = "24.05";
    sshPublicKey = config.userDefinedGlobalVariables.sshPublicKeys.home-desktop.key;
    syncthing = {
      devicesToShareTaskWarriorFolderWith = [
        "${config.userDefinedGlobalVariables.machines.homelab}"
        "${config.userDefinedGlobalVariables.machines.work-pc}"
      ];
      devicesToShareDevResourcesFolderWith = [
        "${config.userDefinedGlobalVariables.machines.homelab}"
        "${config.userDefinedGlobalVariables.machines.work-pc}"
      ];
      devicesToShareDatabaseFolderWith = [
        "${config.userDefinedGlobalVariables.machines.homelab}"
      ];
      devicesToShareDocumentsFolderWith = [
        "${config.userDefinedGlobalVariables.machines.homelab}"
      ];
      devicesToShareStudyFolderWith = [
        "${config.userDefinedGlobalVariables.machines.homelab}"
      ];
    };
  };

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
    alacritty
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
