{ ... }:
{

  imports = [
    ../../os/services/xserver.nix
    ../../os/services/udev.nix
    ../../os/services/user/tmux.nix
    ../../os/services/system/moolticuted.nix
    ../../os/services/cups.nix
    ../../os/services/sshd.nix
  ];
}
