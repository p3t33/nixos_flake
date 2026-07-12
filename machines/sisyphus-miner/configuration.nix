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
           __                   __                                __
    .-----|__.-----.--.--.-----|  |--.--.--.-----.______.--------|__.-----.-----.----.
    |__ --|  |__ --|  |  |  _  |     |  |  |__ --|______|        |  |     |  -__|   _|
    |_____|__|_____|___  |   __|__|__|_____|_____|      |__|__|__|__|__|__|_____|__|
                   |_____|__| powered by NixOS
    '';

    virtualization.incus.storagePoolSize = "10GiB";
  };

  virtualisation.incus.enable = true;

  # zfs support for the file-backed Incus pool. sisyphus-miner's root is ext4,
  # so unlike the zfs machines (where disko implies this) we must enable zfs here.
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.forceImportRoot = false;
  networking.hostId = "f21ff191";

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
