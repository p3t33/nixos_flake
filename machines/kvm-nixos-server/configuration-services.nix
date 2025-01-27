{ ... }:
{

  imports = [
    ../../modules/nixos/services/sshd.nix
    ../../modules/nixos/services/user/tmux.nix
    ../../modules/nixos/services/system/moolticuted.nix
    ../../modules/nixos/services/user/watchman.nix
    ../../modules/nixos/services/syncthing/syncthing.nix
    ../../modules/nixos/services/envfs.nix
    ../../modules/nixos/services/wireguard/wg-quick-client.nix
  ];
}
