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

  # The LAN address is assigned through an OPNsense DHCP reservation recorded in
  # modules/nixos/shared/lan-hosts.nix rather than configured on this interface.

  services.xserver.enable = lib.mkForce false;

  virtualisation.incus.enable = true;

  boot.zfs.forceImportRoot = false;
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
      config.custom.shared.sshPublicKeys.home-desktop.key
      config.custom.shared.sshPublicKeys.work-pc.key
    ];
  };

  # Enable automatic login for the user.
  services.getty.autologinUser = hostSpecific.primeUsername;
}
