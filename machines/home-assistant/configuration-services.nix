{ ... }:
{

  imports = [
    ../../modules/nixos/services/sshd.nix
    ../../modules/nixos/services/user/tmux.nix
    ../../modules/nixos/services/user/watchman.nix
    ../../modules/nixos/services/envfs.nix
    ../../modules/nixos/services/home-assistant.nix
    ../../modules/nixos/services/mosquitto.nix
    ../../modules/nixos/services/zigbee2mqtt.nix
    ../../modules/nixos/services/zfs/auto-scrub.nix
  ];
}
