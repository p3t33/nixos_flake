{ ... }:
{

  imports = [
    ../../os/services/xserver.nix
    ../../os/services/user/tmux.nix
    ../../os/services/system/moolticuted.nix
    ../../os/services/avahi.nix # Used for CUPS to automatically discover IPP printers
    ../../os/services/printer.nix
    ../../os/services/user/watchman.nix
  ];
}
