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
    ../../modules/nixos/bootloader/systemd-boot.nix
    ../../modules/meta.nix
    ../../modules/common/host-specification.nix
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

  hostSpecification = {
    primeUsername = "drone";
    hostConfigurationName = machineName;
    systemStateVersion = "25.05";
    motd = ''
      __  powered by NixOS              __
     |  |--.--.--.--------.______.-----|__.--.--.-----.-----.______.-----.-----.----.--.--.-----.----.
     |    <|  |  |        |______|     |  |_   _|  _  |__ --|______|__ --|  -__|   _|  |  |  -__|   _|
     |__|__|\___/|__|__|__|      |__|__|__|__.__|_____|_____|      |_____|_____|__|  \___/|_____|__|
    '';
  };


  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Configure keymap in X11
  services.xserver = {
    xkb = {
      layout = "us,il";
      variant = "";
    };
  };

  users.users.${config.hostSpecification.primeUsername} = {

    # By default will create /etc/ssh/authorized_keys.d/$USER file with this key in it.
    # This key is added for passwordless login and this key is for VM only
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHhh9jOBY5b+mv4CZqO8qr70RzpMHmjKy3P6kS9lP9KS used with virtual machines"
    ];
  };

  # users.users.root.openssh.authorizedKeys.keys = [
  #     "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINs6NNbZ6EaU1x7cem1zqhDYubadH5Uww+K28e6GOmiY Motorola no password"
  # ];

  # Enable automatic login for the user.
  services.getty.autologinUser = config.hostSpecification.primeUsername;

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
