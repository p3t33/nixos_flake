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

  custom = {
    profiles.system = {
      core.enable = true;
      server.enable = true;
    };

    systemStateVersion = "25.05";
    motd.message = ''
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
      config.customGlobal.sshPublicKeys.home-desktop.key
      config.customGlobal.sshPublicKeys.work-pc.key
    ];
  };

  # Enable automatic login for the user.
  services.getty.autologinUser = hostSpecific.primeUsername;
}
