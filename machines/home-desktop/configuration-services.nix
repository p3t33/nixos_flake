{ ... }:
{

  imports = [
    ../../modules/nixos/services/user/tmux.nix
    ../../modules/nixos/services/system/moolticuted.nix
    ../../modules/nixos/services/avahi.nix # Used for CUPS to automatically discover IPP printers
    ../../modules/nixos/services/printer.nix
    ../../modules/nixos/services/user/watchman.nix
    ../../modules/nixos/services/udisks2.nix
    ../../modules/nixos/services/syncthing/syncthing.nix
    ../../modules/nixos/services/sound.nix
    ../../modules/nixos/services/trezor.nix
    ../../modules/nixos/services/envfs.nix
    ../../modules/nixos/services/gui/xserver.nix
    ../../modules/nixos/services/gui/display_manager/display_manager_base.nix
    ../../modules/nixos/services/greenclip.nix
  ];
}
