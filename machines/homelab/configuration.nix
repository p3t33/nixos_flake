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
    ./disko-configuration-extra-hard-dirves.nix
    ../../modules/nixos # imported via default.nix
  ];

    customOptions = {
      enableConfigurationProfile = {
        core = true;
        server = true;
      };

      systemStateVersion = "24.05";

      motd = ''
         ___ ___                      __       __
        |   |   .-----.--------.-----|  .---.-|  |--.
        |.  |   |  _  |        |  -__|  |  _  |  _  |
        |.  _   |_____|__|__|__|_____|__|___._|_____|
        |:  |   | powered by NixOS
        |::.|:. |
        `--- ---'
      '';
    };

  # systemd will create directory on boot(and set ownership and permission) if it doesn't exist yet.
  systemd.tmpfiles.rules = [
    "d ${config.customHostSpecificGlobalOptions.pathToDataDirectory} 0770 ${hostSpecific.primeUsername} ${config.customGlobalOptions.dataGroup} -"
    "d ${config.customHostSpecificGlobalOptions.pathToMediaDirectory} 0770 ${hostSpecific.primeUsername} ${config.customGlobalOptions.mediaGroup} -"
    "d ${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/tv 0770 ${config.services.sonarr.user} ${config.customGlobalOptions.mediaGroup} -"
    "d ${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/movies 0770 ${config.services.radarr.user} ${config.customGlobalOptions.mediaGroup} -"
  ];

  networking.interfaces.eno1.ipv4.addresses = [
    {
      address = "${config.customGlobalOptions.${hostSpecific.hostName}.ip}";
      prefixLength = 24;
    }
  ];

  networking.defaultGateway = "${config.customGlobalOptions.${hostSpecific.hostName}.gateway}";
  networking.nameservers = [ "8.8.8.8" ];

  # Configure keymap in X11
  services.xserver = {
    xkb = {
      variant = "";
      layout = "us";
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

  # iPerf3 testing ports
  networking.firewall.allowedTCPPorts = [ 5201 ];
  networking.firewall.allowedUDPPorts = [ 5201 ];
}
