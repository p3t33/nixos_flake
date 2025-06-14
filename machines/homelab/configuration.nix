{
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
    ./disko-config-extra-hard-dirves.nix
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
    ../../modules/nixos/environment_variables.nix
    ../../modules/nixos/virtualization/docker.nix
    ../../modules/nixos/networking/networkmanager.nix
    ../../modules/nixos/networking/hostname.nix
    ../../modules/nixos/users.nix
    ../../modules/nixos/dictionaries.nix
    ../../modules/nixos/defaults_for_system_build.nix
    ../../modules/nixos/auto_upgrade.nix
    ../../modules/nixos/graphics.nix
    ../../modules/nixos/motd.nix
  ];

  hostSpecification = {
    primeUsername = "kmedrish";
    hostConfigurationName = machineName;
    systemStateVersion = "24.05";
    syncthing = {
      syncDir = "/mnt/data/Sync";
      user = "syncthing";
      simpleFileVersioningForBackUpMachinesOnly = {
        type = "simple";
        params = {
          keep = "5";
          cleanoutDays = "10";
        };
        cleanupIntervalS = 3600;
      };

      devicesToShareTaskWarriorFolderWith = [
        "${config.userDefinedGlobalVariables.machines.work-pc}"
        "${config.userDefinedGlobalVariables.machines.home-desktop}"
      ];

      devicesToShareDevResourcesFolderWith = [
        "${config.userDefinedGlobalVariables.machines.work-pc}"
        "${config.userDefinedGlobalVariables.machines.home-desktop}"
      ];

      devicesToShareDatabaseFolderWith = [
        "${config.userDefinedGlobalVariables.machines.home-desktop}"
      ];

      devicesToShareDocumentsFolderWith = [
        "${config.userDefinedGlobalVariables.machines.home-desktop}"
      ];

      devicesToShareStudyFolderWith = [
        "${config.userDefinedGlobalVariables.machines.home-desktop}"
      ];
    };
    motd = ''
         ___ ___                      __       __
        |   |   .-----.--------.-----|  .---.-|  |--.
        |.  |   |  _  |        |  -__|  |  _  |  _  |
        |.  _   |_____|__|__|__|_____|__|___._|_____|
        |:  |   | powered by NixOS
        |::.|:. |
        `--- ---'
      '';
  };

  # systemd will create directory on boot(and set ownership and permission) if it doesn't exist yet.
  systemd.tmpfiles.rules = [
    "d ${config.userDefinedGlobalVariables.pathToDataDirectory} 0770 ${config.hostSpecification.primeUsername} ${config.userDefinedGlobalVariables.dataGroup} -"
    "d ${config.userDefinedGlobalVariables.pathToMediaDirectory} 0770 ${config.hostSpecification.primeUsername} ${config.userDefinedGlobalVariables.mediaGroup} -"
    "d ${config.userDefinedGlobalVariables.pathToMediaDirectory}/tv 0770 ${config.services.sonarr.user} ${config.userDefinedGlobalVariables.mediaGroup} -"
    "d ${config.userDefinedGlobalVariables.pathToMediaDirectory}/movies 0770 ${config.services.radarr.user} ${config.userDefinedGlobalVariables.mediaGroup} -"
  ];

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

  users.users.${config.hostSpecification.primeUsername} = {

    # By default will create /etc/ssh/authorized_keys.d/$USER file with this key in it.
    # This key is added for passwordless login and this key is for VM only
    openssh.authorizedKeys.keys = [
      config.userDefinedGlobalVariables.sshPublicKeys.home-desktop.key
      config.userDefinedGlobalVariables.sshPublicKeys.work-pc.key
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget

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
