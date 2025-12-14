{ config, lib, ... }:

let
  g = config.custom.profiles.systemServices;
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
    ./restic.nix
    ./sabnzbd.nix
    ./samba.nix
    ./sonarr.nix
    ./sound.nix
    ./sshd.nix
    ./trezor.nix
    ./zigbee2mqtt.nix

    ./gui/xserver.nix
    ./gui/display_manager/display_manager_base.nix

    ./system/moolticuted.nix

    ./user/tmux.nix
    ./user/watchman.nix
    ./user/sxhkd.nix

    ./user/torrent/deluge.nix
    ./user/torrent/qbittorrent-nox.nix

    ./wireguard/wg-quick-client.nix
    ./wireguard/wireguard-server.nix

    ./zfs/auto-scrub.nix

    ./syncthing
    ./postgresql/postgresql.nix
    ./postgresql/postgresql_backup.nix
    ./gvfs.nix

    # mining
    ./monerod.nix
    ./p2pool.nix
    ./xmrig.nix

    ./imminch.nix
  ];

  options.custom.profiles.systemServices = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.enable = lib.mkEnableOption "Enable this system service profile";
    });
    default = {};
    description = "Enable system profiles like 'core', 'server', etc.";
  };

     config  = lib.mkMerge [

    # Core profile enables CLI, dev, encryption
    (lib.mkIf (g.core.enable or false) {
      custom.services.tmuxd.enable = true;
      custom.services.watchman.enable = true;
    })

    # Desktop profile enables GUI etc.
    (lib.mkIf (g.desktop.enable or false) {
      services.avahi.enable = true;
      services.printing.enable = true;
      services.udisks2.enable = true; # mount and unmout USB drives
      services.pipewire.enable = true; # sound
      services.xserver.enable = true;
      custom.services.displayManager.enable = true; #todo
      services.upower.enable = true; # used by polybar battery module.
      services.greenclip.enable = true; # clipboard manager
      custom.services.sxhkd.enable = true;
      custom.services.moolticuted.enable = true;
    })

    # Server profile enables minimal features
    (lib.mkIf (g.server.enable or false) {
      services.openssh.enable = true; # sshd
      services.fail2ban.enable = true;
    })

    (lib.mkIf (g.xmr-miner.enable or false) {
      services.monero.enable = true;
      custom.services.p2pool.enable = true;
      services.xmrig.enable = true;
    })

    (lib.mkIf (g.wireguardServer.enable or false) {
      custom.vpn.wireguardServer.enable = true;
      services.inadyn.enable = true;
    })
  ];
}
