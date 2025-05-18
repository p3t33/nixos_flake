{ ... }:
{
  imports = [
    ../../modules/nixos/services/sshd.nix
    ../../modules/nixos/services/user/tmux.nix
    ../../modules/nixos/services/system/moolticuted.nix
    ../../modules/nixos/services/user/watchman.nix
    ../../modules/nixos/services/user/torrent/qbittorrent-nox.nix # not enabled
    ../../modules/nixos/services/user/torrent/deluge.nix
    ../../modules/nixos/services/syncthing/syncthing.nix
    ../../modules/nixos/services/restic.nix
    ../../modules/nixos/services/homepage_dashboard.nix
    ../../modules/nixos/services/jellyfin.nix
    ../../modules/nixos/services/nginx.nix
    ../../modules/nixos/services/fail2ban.nix
    ../../modules/nixos/services/envfs.nix
    ../../modules/nixos/services/adguard_home.nix
    ../../modules/nixos/services/prowlarr.nix
    ../../modules/nixos/services/jackett.nix
    ../../modules/nixos/services/sonarr.nix
    ../../modules/nixos/services/radarr.nix
    ../../modules/nixos/services/readarr.nix
    ../../modules/nixos/services/wireguard/wireguard-server.nix
    ../../modules/nixos/services/bazarr.nix
    ../../modules/nixos/services/inadyn.nix
    ../../modules/nixos/services/prometheus.nix
    ../../modules/nixos/services/promtail.nix
    ../../modules/nixos/services/loki.nix
    ../../modules/nixos/services/grafana.nix
    ../../modules/nixos/services/samba.nix
    ../../modules/nixos/services/sabnzbd.nix
    ../../modules/nixos/services/paperless.nix
    ../../modules/nixos/services/calibre-web.nix
  ];
}
