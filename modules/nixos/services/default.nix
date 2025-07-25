{ config, lib, ... }:

let
  g = config.customOptions.enableServicesProfile;
in

{
  imports = [
    ./adb.nix
    ./adguard_home.nix
    ./avahi.nix
    ./bazarr.nix
    ./bluetooth.nix
    ./calibre-web.nix
    ./envfs.nix # always enabled
    ./fail2ban.nix
    ./gatus.nix
    ./grafana.nix
    ./greenclip.nix
    ./home-assistant.nix
    ./homepage_dashboard.nix
    ./inadyn.nix
    ./jackett.nix
    ./jellyfin.nix
    ./loki.nix
    ./mosquitto.nix
    ./nginx.nix
    ./paperless.nix
    ./printer.nix
    ./prometheus.nix
    ./promtail.nix
    ./prowlarr.nix
    ./radarr.nix
    ./readarr.nix
    ./restic.nix
    ./sabnzbd.nix
    ./samba.nix
    ./sonarr.nix
    ./sound.nix
    ./sshd.nix
    ./trezor.nix
    ./udisks2.nix
    ./upower.nix
    ./zigbee2mqtt.nix

    ./gui/xserver.nix
    ./gui/display_manager/display_manager_base.nix

    ./system/moolticuted.nix

    ./user/tmux.nix # bind to programs.tmux.enable
    ./user/watchman.nix
    ./user/sxhkd.nix

    ./user/torrent/deluge.nix
    ./user/torrent/qbittorrent-nox.nix

    ./wireguard/wg-quick-client.nix
    ./wireguard/wireguard-server.nix

    ./zfs/auto-scrub.nix

    ./syncthing/syncthing.nix
    ./postgresql/postgresql.nix
    ./postgresql/postgresql_backup.nix

    ./monerod.nix
  ];

    options.customOptions.enableServicesProfile = lib.mkOption {
      type = lib.types.attrsOf lib.types.bool;
      default = {};
      description = "Enable system profiles like 'desktop', 'server', 'securityKeys', etc.";
    };

     config.customOptions.enableModule = lib.mkMerge [

    # Core profile enables CLI, dev, encryption
    (lib.mkIf (g.core or false) {
      tmuxService = true;
      watchman = true;
    })

    # Desktop profile enables GUI etc.
    (lib.mkIf (g.desktop or false) {
      avahi = true;
      printer = true;
      udisks2 = true;
      sound = true;
      xserver = true;
      displayManager = true;
      upower = true;
      greenclip = true;
      sxhkd = true;
      moolticuted = true;
    })

    # Server profile enables minimal features
    (lib.mkIf (g.server or false) {
      sshd  = true;
      fail2ban = true;
    })

    (lib.mkIf (g.xmr-miner or false) {
      monerod  = true;
    })
  ];

}
