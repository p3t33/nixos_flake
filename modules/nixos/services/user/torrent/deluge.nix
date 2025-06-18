{ config, pkgs, ... }:
{
  services.deluge = {
    enable = true;
    group = "${config.customGlobalOptions.mediaGroup}";
    # Only works when declarative = true and only for non random ports.
    openFirewall = true;
    web = {
      enable = true;
      openFirewall = true;
      port = 8112;
    };
    extraPackages = with pkgs; [
      unzip
      gnutar
      xz
      bzip2
      unrar
      p7zip # dependencies for Extractor plugin.
    ];

    declarative = true;
    config = {
      # /mnt/media is a mount point with defined owner, and permissions
      # so I was getting warning when NixOS was switching.
      download_location = "${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/torrents";
      max_active_seeding = 200;
      max_active_downloading = 200;
      max_active_limit = 200;
      allow_remote = false;
      dht = false;
      upnp = false;
      natpmp = false;
      utpex = false;
      lsd = false;

      # If this isn't set to false, deluge will ignore the ports
      # set by listen_ports and just pick a random port. meaning
      # this randmo port won't have firewall opened for it and so
      # incoming traffic will not work.
      random_port = false;
      # Set a fixed range for P2P communication ports, minumum two so I used the same value.
      # Same ports need to be opened in the router firewall.
      listen_ports = [
        6629
        6629
      ];
      enabled_plugins = [
        "Extractor"
        "Label"
      ];
    };

    # has to be defined for "declarative = true"
    # defines the clients that can access the deluged, this is requried for
    # the webgui and anybody else who needs access(such as prometheus exporter).
    authFile = config.sops.secrets.deluge_auth_file.path;
  };

  sops.secrets.deluge_auth_file = {
    mode = "0660";
    group = "${config.customGlobalOptions.mediaGroup}";
  };

}
