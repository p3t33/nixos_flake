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

    systemStateVersion = "25.11";
    motd.message = ''
        __          __
       |  |--.-----|  .-----.-----.----.
       |     |  -__|  |  _  |  -__|   _|
       |__|__|_____|__|   __|_____|__|
                      |__| powered by NixOS
    '';
  };

  services.xserver = {
    xkb = {
      layout = "us,il";
      variant = "";
    };
  };

  users.users.${hostSpecific.primeUsername} = {
    openssh.authorizedKeys.keys = [
      config.custom.shared.sshPublicKeys.home-desktop.key
      config.custom.shared.sshPublicKeys.work-pc.key
    ];
  };

  networking.hostId = "48ca4565";

  services.getty.autologinUser = hostSpecific.primeUsername;
}
