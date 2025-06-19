{
  config,
  hostSpecific,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ./services-configuration.nix
    ./disko-configuration.nix
    ./sops-configuration.nix
    ../../modules/nixos # imported via default.nix
  ];

  customOptions = {
    enableConfigurationProfile = {
      core = true;
      server = true;
    };

    enableModule = {
      qemuGuest = true;
    };

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
