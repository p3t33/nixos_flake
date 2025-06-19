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
    ./readarr.nix
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

    ./syncthing/syncthing.nix
    ./postgresql/postgresql.nix
    ./postgresql/postgresql_backup.nix
  ];

    options.custom.profiles.systemServices = lib.mkOption {
      type = lib.types.attrsOf lib.types.bool;
      default = {};
      description = "Enable system profiles like 'desktop', 'server', 'securityKeys', etc.";
    };

     config  = lib.mkMerge [

    # Core profile enables CLI, dev, encryption
    (lib.mkIf (g.core or false) {
      custom.services.tmuxd.enable = true;
      custom.services.watchman.enable = true;
    })

    # Desktop profile enables GUI etc.
    (lib.mkIf (g.desktop or false) {
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
    (lib.mkIf (g.server or false) {
      services.openssh.enable = true; # sshd
      services.fail2ban.enable = true;
    })
  ];

}
