{
  config,
  hostSpecific,
  lib,
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
      .-----.---.-.-----.
      |     |  _  |__ --|
      |__|__|___._|_____|
         powered by NixOS
    '';
  };

  services.xserver.enable = lib.mkForce false;
  system.stateVersion = "25.05";
  networking.hostId = "b8835c95";

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
