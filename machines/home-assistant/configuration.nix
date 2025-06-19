{
  config,
  hostSpecific,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ./services-configuration.nix
    ./global-options.nix
    ./sops-configuration.nix
    ./disko-configuration.nix
    ../../modules/nixos # imported via default.nix
  ];

    # openssl rand -hex 4
    networking.hostId = "cf97b6ff";

    customOptions = {
      enableConfigurationProfile = {
        core = true;
        server = true;
      };

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
