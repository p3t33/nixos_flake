{ config, ... }:
{
  services.deluge = {
    enable = true;
    group = "media";
    openFirewall = true;
    web = {
      enable = true;
      openFirewall = true;
    };

    declarative = true;
    config = {
      # /mnt/media is a mount point with defined owner, and permissions
      # so I was getting warning when NixOS was switching.
      download_location = "/mnt/media/torrents";
      max_active_seeding = 200;
      max_active_downloading = 200;
      max_active_limit = 200;
      allow_remote = false;
      dht = false;
      upnp = false;
      natpmp = false;
      utpex = false;
      lsd = false;
    };
    # has to be defined for "declarative = true"
    authFile = config.sops.secrets.deluge_auth_file.path;

  };

  sops.secrets.deluge_auth_file = {
    mode = "0660";
    group = "media";
  };

}
