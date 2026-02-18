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
    ./n8n.nix
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
    ./ollama.nix
  ];

  options.custom.profiles.systemServices = {
    core.enable = lib.mkEnableOption "core service profile (tmux daemon, watchman)";
    desktop.enable = lib.mkEnableOption "desktop service profile (sound, printing, display manager, etc.)";
    server.enable = lib.mkEnableOption "server service profile (sshd, fail2ban)";
    xmr-miner.enable = lib.mkEnableOption "XMR miner service profile (monerod, p2pool, xmrig)";
    wireguardServer.enable = lib.mkEnableOption "WireGuard server service profile (wireguard, inadyn)";
  };

  config = lib.mkMerge [
    (lib.mkIf g.core.enable {
      custom.services.tmuxd.enable = true;
      custom.services.watchman.enable = true;
    })

    (lib.mkIf g.desktop.enable {
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

    (lib.mkIf g.server.enable {
      services.openssh.enable = true; # sshd
      services.fail2ban.enable = true;
    })

    (lib.mkIf g.xmr-miner.enable {
      services.monero.enable = true;
      custom.services.p2pool.enable = true;
      services.xmrig.enable = true;
    })

    (lib.mkIf g.wireguardServer.enable {
      custom.vpn.wireguardServer.enable = true;
      services.inadyn.enable = true;
    })
  ];
}
