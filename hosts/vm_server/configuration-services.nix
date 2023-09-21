{ ... }:
{

  imports = [
    ../../os/services/sshd.nix
    ../../os/services/user/tmux.nix
    ../../os/services/system/moolticuted.nix
    ../../os/services/user/watchman.nix
  ];

}
