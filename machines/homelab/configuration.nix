{
  config,
  hostSpecific,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ./configuration-services.nix
    ./global-options.nix
    ./sops-configuration.nix
    ./disko-config.nix
    ./disko-config-extra-hard-dirves.nix
    ../../modules/nixos/bootloader/systemd-boot.nix
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
    ../../modules/nixos/defaults_for_system_build.nix
    ../../modules/nixos/auto_upgrade.nix
    ../../modules/nixos/graphics.nix
    ../../modules/nixos/motd.nix
  ];

    customOptions = {
      systemStateVersion = "24.05";

      motd = ''
         ___ ___                      __       __
        |   |   .-----.--------.-----|  .---.-|  |--.
        |.  |   |  _  |        |  -__|  |  _  |  _  |
        |.  _   |_____|__|__|__|_____|__|___._|_____|
        |:  |   | powered by NixOS
        |::.|:. |
        `--- ---'
      '';

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
          "${config.customOptions.syncthing.devices.work-pc}"
          "${config.customOptions.syncthing.devices.home-desktop}"
         ];

        devicesToShareDevResourcesFolderWith = [
          "${config.customOptions.syncthing.devices.work-pc}"
          "${config.customOptions.syncthing.devices.home-desktop}"
        ];

        devicesToShareDatabaseFolderWith = [
          "${config.customOptions.syncthing.devices.home-desktop}"
        ];

        devicesToShareDocumentsFolderWith = [
          "${config.customOptions.syncthing.devices.home-desktop}"
        ];

        devicesToShareStudyFolderWith = [
          "${config.customOptions.syncthing.devices.home-desktop}"
        ];
      };
    };

  # systemd will create directory on boot(and set ownership and permission) if it doesn't exist yet.
  systemd.tmpfiles.rules = [
    "d ${config.customHostSpecificGlobalOptions.pathToDataDirectory} 0770 ${hostSpecific.primeUsername} ${config.customGlobalOptions.dataGroup} -"
    "d ${config.customHostSpecificGlobalOptions.pathToMediaDirectory} 0770 ${hostSpecific.primeUsername} ${config.customGlobalOptions.mediaGroup} -"
    "d ${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/tv 0770 ${config.services.sonarr.user} ${config.customGlobalOptions.mediaGroup} -"
    "d ${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/movies 0770 ${config.services.radarr.user} ${config.customGlobalOptions.mediaGroup} -"
  ];

  networking.interfaces.eno1.ipv4.addresses = [
    {
      address = "${config.customGlobalOptions.${hostSpecific.hostName}.ip}";
      prefixLength = 24;
    }
  ];

  networking.defaultGateway = "${config.customGlobalOptions.${hostSpecific.hostName}.gateway}";
  networking.nameservers = [ "8.8.8.8" ];

  # Configure keymap in X11
  services.xserver = {
    xkb = {
      variant = "";
      layout = "us";
    };
  };

  users.users.${hostSpecific.primeUsername} = {

    # By default will create /etc/ssh/authorized_keys.d/$USER file with this key in it.
    # This key is added for passwordless login and this key is for VM only
    openssh.authorizedKeys.keys = [
      config.customGlobalOptions.sshPublicKeys.home-desktop.key
      config.customGlobalOptions.sshPublicKeys.work-pc.key
    ];
  };

  # iPerf3 testing ports
  networking.firewall.allowedTCPPorts = [ 5201 ];
  networking.firewall.allowedUDPPorts = [ 5201 ];
}
