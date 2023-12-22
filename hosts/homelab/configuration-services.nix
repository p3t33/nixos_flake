{ ... }:
{

  imports = [
    ../../modules/nixos/services/sshd.nix
    ../../modules/nixos/services/user/tmux.nix
    ../../modules/nixos/services/system/moolticuted.nix
    ../../modules/nixos/services/user/watchman.nix
    ../../modules/nixos/services/user/torrent/qbittorrent-nox.nix # not enabled
  ];

}
