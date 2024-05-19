{ ... }:
{

  imports = [
    ../../modules/nixos/services/xserver.nix
    ../../modules/nixos/services/user/tmux.nix
    ../../modules/nixos/services/system/moolticuted.nix
    ../../modules/nixos/services/avahi.nix # Used for CUPS to automatically discover IPP printers
    ../../modules/nixos/services/printer.nix
    ../../modules/nixos/services/sshd.nix
    ../../modules/nixos/services/user/watchman.nix
    ../../modules/nixos/services/udisks2.nix
  ];
}
