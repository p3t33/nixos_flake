{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    arandr
    xorg.xrandr
  ];

  imports = [
    ../../modules/shared.nix
    ./sops-home.nix
    ../../modules/home-manager/core.nix # enables home manger and sets the bare minimum.
    #../../modules/home-manager/shared/colors.nix
    ../../modules/home-manager/session-variables.nix
    ../../modules/home-manager/starship.nix
    ../../modules/home-manager/fzf.nix
    ../../modules/home-manager/neovim # uses default.nix
    # ../../modules/home-manager/git.nix
    ../../modules/home-manager/zoxide.nix
    ../../modules/home-manager/zsh.nix
    ../../modules/home-manager/bash.nix
    ../../modules/home-manager/tmux.nix
    #../../modules/home-manager/sxhkd.nix
    #../../modules/home-manager/syncthing.nix
  #  ../../modules/home-manager/i3status.nix
    ../../modules/home-manager/picom.nix
  #  ../../modules/home-manager/i3.nix
  #  ../../modules/home-manager/rofi.nix
    ../../modules/home-manager/gpg.nix
   ../../modules/home-manager/ssh/ssh-client.nix
  #  ../../modules/home-manager/polybar.nix
   # ../../modules/home-manager/scripts/rofi-buku-bookmarks.nix
  #  ../../modules/home-manager/scripts/helloworld-python.nix
   ../../modules/home-manager/scripts/tmux-sessionizer.nix
   ../../modules/home-manager/scripts/cheat-sh.nix
  #  ../../modules/home-manager/redshift.nix
   ../../modules/home-manager/taskwarrior.nix
  #  ../../modules/home-manager/wallpaper.nix
   ../../modules/home-manager/atuin.nix
   ../../modules/home-manager/git/lazygit.nix
  #  ../../modules/home-manager/emacs # uses default.nix
  #  ../../modules/home-manager/xdg/mime_apps.nix
  #  ../../modules/home-manager/gtk.nix
  #  ../../modules/home-manager/qt.nix
  #  ../../modules/home-manager/services/dunst.nix
   ../../modules/home-manager/bat.nix
  #  ../../modules/home-manager/firefox.nix
  #  ../../modules/home-manager/generic_linux_fonts.nix
   ../../modules/home-manager/yazi.nix
  ];

  programs.starship.enable = true;
  programs.fzf.enable = true;
  programs.neovim.enable = true;
  programs.zoxide.enable = true;
  programs.atuin.enable = true;
  programs.zsh.enable = true;
  programs.bash.enable = true;
  programs.tmux.enable = true;
  programs.ssh.enable = true;
  programs.taskwarrior.enable = true;
  programs.lazygit.enable = true;
  programs.bat.enable = true;
  programs.yazi.enable = true;
  programs.gpg.enable = true;

  custom.shared.sopsKeyPath = "${config.custom.shared.primeUserHomeDirectory }/.config/sops/age/keys.txt";
  targets.genericLinux.enable = true;
}
