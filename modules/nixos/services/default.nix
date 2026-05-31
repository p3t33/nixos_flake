{ config, lib, ... }:

let
  g = config.custom.profiles.systemServices;
in
{
  imports = [
    ./adguard_home.nix
    ./avahi.nix
    ./bluetooth.nix
    ./calibre-web.nix
    ./envfs.nix # always enabled
    ./nix-ld.nix
    ./fail2ban.nix
    ./gatus.nix
    ./grafana
    ./home-assistant
    ./homepage_dashboard.nix
    ./download-clients
    ./media
    ./inadyn.nix
    ./loki.nix
    ./mosquitto.nix
    ./nginx.nix
    ./paperless.nix
    ./n8n.nix
    ./printer.nix
    ./prometheus.nix
    ./alloy.nix
    ./restic.nix
    ./samba.nix
    ./sound.nix
    ./sshd.nix
    ./trezor.nix
    ./zigbee2mqtt.nix

    ./gui/xserver.nix
    ./gui/display_manager/display_manager_base.nix

    ./system/moolticuted.nix

    ./user/tmux.nix
    ./user/mutagen.nix
    ./user/watchman.nix
    ./user/sxhkd.nix


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

    ./immich.nix
    ./ollama.nix
    ./minecraft.nix
  ];

  options.custom.profiles.systemServices = {
    core.enable = lib.mkEnableOption "core service profile (tmux daemon, watchman)";
    desktop.enable = lib.mkEnableOption "desktop service profile (sound, printing, display manager, etc.)";
    server.enable = lib.mkEnableOption "server service profile (sshd, fail2ban)";
    xmr-miner.enable = lib.mkEnableOption "XMR miner service profile (monerod, p2pool, xmrig)";
    wireguardServer.enable = lib.mkEnableOption "WireGuard server service profile (wireguard, inadyn)";
    monitoring.enable = lib.mkEnableOption "monitoring service profile (prometheus, loki, alloy, grafana)";
  };

  config = lib.mkMerge [
    (lib.mkIf g.core.enable {
      custom.services.tmuxd.enable = true;
      custom.services.mutagen.enable = true;
      custom.services.watchman.enable = true;
    })

    (lib.mkIf g.desktop.enable {
      services.avahi.enable = true;
      custom.services.printing.client.enable = true;
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
      services.openssh.enable  = true; # sshd
      services.fail2ban.enable = true;
    })

    (lib.mkIf g.xmr-miner.enable {
      services.monero.enable = true;
      custom.services.p2pool.enable = true;
      services.xmrig.enable = true;
    })

    (lib.mkIf g.monitoring.enable {
      services.prometheus.enable = true;
      services.loki.enable = true;
      services.alloy.enable = true;
      services.grafana.enable = true;
    })

    (lib.mkIf g.wireguardServer.enable {
      custom.vpn.wireguardServer.enable = true;
      services.inadyn.enable = true;
    })
  ];
}
