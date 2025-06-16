{
  config,
  hostSpecific,
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
    ../../modules/nixos/virtualization/qemu_guest_agent.nix
    ../../modules/nixos/networking/networkmanager.nix
    ../../modules/nixos/networking/hostname.nix
    ../../modules/nixos/users.nix
    ../../modules/nixos/dictionaries.nix
    ../../modules/nixos/defaults_for_system_build.nix
    ../../modules/nixos/graphics.nix
    ../../modules/nixos/motd.nix
  ];

  customOptions = {
    systemStateVersion = "25.05";
    motd = ''
      __  powered by NixOS              __
     |  |--.--.--.--------.______.-----|__.--.--.-----.-----.______.-----.-----.----.--.--.-----.----.
     |    <|  |  |        |______|     |  |_   _|  _  |__ --|______|__ --|  -__|   _|  |  |  -__|   _|
     |__|__|\___/|__|__|__|      |__|__|__|__.__|_____|_____|      |_____|_____|__|  \___/|_____|__|
    '';
  };

  system.stateVersion = "25.05";

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
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHhh9jOBY5b+mv4CZqO8qr70RzpMHmjKy3P6kS9lP9KS used with virtual machines"
    ];
  };

  # Enable automatic login for the user.
  services.getty.autologinUser = hostSpecific.primeUsername;
}
