{ ... }:
{

  imports = [
    ../services/xserver.nix
    ../services/udev.nix
    ../services/user/tmux.nix
    ../services/user/sxhkd.nix
    ../services/system/moolticuted.nix
    ../services/cups.nix
  ];
}
