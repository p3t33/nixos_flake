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
    ../../modules/nixos/bootloader/systemd-boot.nix
    ../../modules/meta.nix
    ../../modules/nixos/home-manager-as-nixos-module.nix
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
    ../../modules/nixos/graphics.nix
    ../../modules/nixos/auto_upgrade.nix
    ../../modules/nixos/motd.nix
  ];

    # openssl rand -hex 4
    networking.hostId = "cf97b6ff";

    customOptions = {
      systemStateVersion = "24.11";
      motd = ''
         ___ ___                             _______             __       __               __
        |   |   .-----.--------.-----.______|   _   .-----.-----|__.-----|  |_.---.-.-----|  |_
        |.  |   |  _  |        |  -__|______|.  |   |__ --|__ --|  |__ --|   _|  _  |     |   _|
        |.  _   |_____|__|__|__|_____|      |.  _   |_____|_____|__|_____|____|___._|__|__|____|
        |:  |   | powered by NixOS          |:  |   |
        |::.|:. |                           |::.|:. |
        `--- ---'                           `--- ---'
      '';
    };

  # Configure keymap in X11
  services.xserver = {
    xkb = {
      layout = "us,il";
      variant = "";
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

  # Enable automatic login for the user.
  services.getty.autologinUser = hostSpecific.primeUsername;
}
